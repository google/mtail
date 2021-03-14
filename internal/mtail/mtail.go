// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"context"
	"expvar"
	"net"
	"net/http"
	"net/http/pprof"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/exporter"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/tailer"
	"github.com/google/mtail/internal/vm"
	"github.com/google/mtail/internal/waker"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/prometheus/common/version"
	"go.opencensus.io/zpages"
)

// Server contains the state of the main mtail program.
type Server struct {
	ctx   context.Context
	store *metrics.Store // Metrics storage
	wg    sync.WaitGroup // wait for main processes to shutdown

	t *tailer.Tailer     // t manages log patterns and log streams, which sends lines to the VMs
	l *vm.Loader         // l loads programs and manages the VM lifecycle
	e *exporter.Exporter // e manages the export of metrics from the store

	lines chan *logline.LogLine // primary communication channel, owned by Tailer.

	reg *prometheus.Registry

	listener net.Listener // Configured with bind address.

	buildInfo          BuildInfo // go build information
	programPath        string    // path to programs to load
	logPathPatterns    []string  // list of patterns to watch for log files to tail
	ignoreRegexPattern string

	oneShot      bool // if set, mtail reads log files from the beginning, once, then exits
	compileOnly  bool // if set, mtail compiles programs then exits
	dumpAst      bool // if set, mtail prints the program syntax tree after parse
	dumpAstTypes bool // if set, mtail prints the program syntax tree after type checking
	dumpBytecode bool // if set, mtail prints the program bytecode after code generation

	overrideLocation     *time.Location // Timezone location to use when parsing timestamps
	staleLogGcWaker      waker.Waker    // Wake to run stale log gc
	logPatternPollWaker  waker.Waker    // Wake to poll for log patterns
	logstreamPollWaker   waker.Waker    // Wake idle logstreams to poll sfor new data
	metricPushInterval   time.Duration  // Interval between metric pushes
	syslogUseCurrentYear bool           // if set, use the current year for timestamps that have no year information
	omitMetricSource     bool           // if set, do not link the source program to a metric
	omitProgLabel        bool           // if set, do not put the program name in the metric labels
	emitMetricTimestamp  bool           // if set, emit the metric's recorded timestamp
}

// initLoader constructs a new program loader and performs the initial load of program files in the program directory.
func (m *Server) initLoader() error {
	opts := []vm.Option{
		vm.PrometheusRegisterer(m.reg),
	}
	if m.compileOnly {
		opts = append(opts, vm.CompileOnly())
	}
	if m.oneShot {
		opts = append(opts, vm.ErrorsAbort())
	}
	if m.dumpAst {
		opts = append(opts, vm.DumpAst())
	}
	if m.dumpAstTypes {
		opts = append(opts, vm.DumpAstTypes())
	}
	if m.dumpBytecode {
		opts = append(opts, vm.DumpBytecode())
	}
	if m.syslogUseCurrentYear {
		opts = append(opts, vm.SyslogUseCurrentYear())
	}
	if m.omitMetricSource {
		opts = append(opts, vm.OmitMetricSource())
	}
	if m.overrideLocation != nil {
		opts = append(opts, vm.OverrideLocation(m.overrideLocation))
	}
	var err error
	m.l, err = vm.NewLoader(m.lines, &m.wg, m.programPath, m.store, opts...)
	if err != nil {
		return err
	}
	return nil
}

// initExporter sets up an Exporter for this Server.
func (m *Server) initExporter() (err error) {
	if m.oneShot {
		// This is a hack to avoid a race in test, but assume that in oneshot
		// mode we don't want to export anything.
		return nil
	}
	opts := []exporter.Option{}
	if m.omitProgLabel {
		opts = append(opts, exporter.OmitProgLabel())
	}
	if m.emitMetricTimestamp {
		opts = append(opts, exporter.EmitTimestamp())
	}
	if m.metricPushInterval > 0 {
		opts = append(opts, exporter.PushInterval(m.metricPushInterval))
	}
	m.e, err = exporter.New(m.ctx, &m.wg, m.store, opts...)
	if err != nil {
		return err
	}
	m.reg.MustRegister(m.e)

	// Create mtail_build_info metric.
	version.Branch = m.buildInfo.Branch
	version.Version = m.buildInfo.Version
	version.Revision = m.buildInfo.Revision
	m.reg.MustRegister(version.NewCollector("mtail"))
	return nil
}

// initTailer sets up and starts a Tailer for this Server.
func (m *Server) initTailer() (err error) {
	opts := []tailer.Option{
		tailer.IgnoreRegex(m.ignoreRegexPattern),
		tailer.LogPatterns(m.logPathPatterns),
		tailer.LogPatternPollWaker(m.logPatternPollWaker),
		tailer.StaleLogGcWaker(m.staleLogGcWaker),
		tailer.LogstreamPollWaker(m.logstreamPollWaker),
	}
	if m.oneShot {
		opts = append(opts, tailer.OneShot)
	}
	m.t, err = tailer.New(m.ctx, &m.wg, m.lines, opts...)
	return
}

// initHttpServer begins the http server.
func (m *Server) initHttpServer() error {
	initDone := make(chan struct{})
	defer close(initDone)

	if m.listener == nil {
		glog.Info("no listen address configured, not starting http server")
		return nil
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/favicon.ico", FaviconHandler)
	mux.Handle("/", m)
	mux.Handle("/progz", http.HandlerFunc(m.l.ProgzHandler))
	mux.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	mux.Handle("/metrics", promhttp.HandlerFor(m.reg, promhttp.HandlerOpts{}))
	mux.HandleFunc("/graphite", http.HandlerFunc(m.e.HandleGraphite))
	mux.HandleFunc("/varz", http.HandlerFunc(m.e.HandleVarz))
	mux.Handle("/debug/vars", expvar.Handler())
	mux.HandleFunc("/debug/pprof/", pprof.Index)
	mux.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)
	mux.HandleFunc("/debug/pprof/profile", pprof.Profile)
	mux.HandleFunc("/debug/pprof/symbol", pprof.Symbol)
	mux.HandleFunc("/debug/pprof/trace", pprof.Trace)
	zpages.Handle(mux, "/")

	srv := &http.Server{
		Handler: mux,
	}

	var wg sync.WaitGroup
	errc := make(chan error, 1)

	// This goroutine runs the http server.
	wg.Add(1)
	go func() {
		defer wg.Done()
		<-initDone
		glog.Infof("Listening on %s", m.listener.Addr())
		if err := srv.Serve(m.listener); err != nil && err != http.ErrServerClosed {
			errc <- err
		}
	}()

	// This goroutine manages http server shutdown.
	go func() {
		<-initDone
		select {
		case err := <-errc:
			glog.Info(err)
		case <-m.ctx.Done():
			glog.Info("Shutdown requested.")
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			srv.SetKeepAlivesEnabled(false)
			if err := srv.Shutdown(ctx); err != nil {
				glog.Info(err)
			}
		}
		// Wait for the Serve routine to exit.
		wg.Wait()
	}()

	return nil
}

// New creates a Server from the supplied Options.  The Server is started by
// the time New returns, it watches the LogPatterns for files, starts tailing
// their changes and sends any new lines found to the virtual machines loaded
// from ProgramPath. If OneShot mode is enabled, it will exit after reading
// each log file from start to finish.
// TODO(jaq): this doesn't need to be a constructor anymore, it could start and
// block until quit, once TestServer.PollWatched is addressed.
func New(ctx context.Context, store *metrics.Store, options ...Option) (*Server, error) {
	m := &Server{
		ctx:   ctx,
		store: store,
		lines: make(chan *logline.LogLine),
		// Using a non-pedantic registry means we can be looser with metrics that
		// are not fully specified at startup.
		reg: prometheus.NewRegistry(),
	}

	// TODO(jaq): Should these move to initExporter?
	expvarDescs := map[string]*prometheus.Desc{
		// internal/tailer/file.go
		"log_errors_total":    prometheus.NewDesc("log_errors_total", "number of IO errors encountered per log file", []string{"logfile"}, nil),
		"log_rotations_total": prometheus.NewDesc("log_rotations_total", "number of log rotation events per log file", []string{"logfile"}, nil),
		"log_truncates_total": prometheus.NewDesc("log_truncates_total", "number of log truncation events log file", []string{"logfile"}, nil),
		"log_lines_total":     prometheus.NewDesc("log_lines_total", "number of lines read per log file", []string{"logfile"}, nil),
		// internal/vm/loader.go
		"lines_total":               prometheus.NewDesc("lines_total", "number of lines received by the program loader", nil, nil),
		"prog_loads_total":          prometheus.NewDesc("prog_loads_total", "number of program load events by program source filename", []string{"prog"}, nil),
		"prog_load_errors_total":    prometheus.NewDesc("prog_load_errors_total", "number of errors encountered when loading per program source filename", []string{"prog"}, nil),
		"prog_runtime_errors_total": prometheus.NewDesc("prog_runtime_errors_total", "number of errors encountered when executing programs per source filename", []string{"prog"}, nil),
	}
	m.reg.MustRegister(
		prometheus.NewGoCollector(),
		prometheus.NewProcessCollector(prometheus.ProcessCollectorOpts{}))
	// Prefix all expvar metrics with 'mtail_'
	prometheus.WrapRegistererWithPrefix("mtail_", m.reg).MustRegister(
		prometheus.NewExpvarCollector(expvarDescs))
	if err := m.SetOption(options...); err != nil {
		return nil, err
	}
	if err := m.initExporter(); err != nil {
		return nil, err
	}
	if err := m.initLoader(); err != nil {
		return nil, err
	}
	if err := m.initTailer(); err != nil {
		return nil, err
	}
	if err := m.initHttpServer(); err != nil {
		return nil, err
	}
	return m, nil
}

// SetOption takes one or more option functions and applies them in order to MtailServer.
func (m *Server) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option.apply(m); err != nil {
			return err
		}
	}
	return nil
}

// Run awaits mtail's shutdown.
// TODO(jaq): remove this once the test server is able to trigger polls on the components.
func (m *Server) Run() error {
	m.wg.Wait()
	if m.compileOnly {
		glog.Info("compile-only is set, exiting")
		return nil
	}
	return nil
}
