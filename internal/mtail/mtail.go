// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"context"
	"encoding/json"
	"expvar"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/pprof"
	"os"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/exporter"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/tailer"
	"github.com/google/mtail/internal/vm"
	"github.com/google/mtail/internal/watcher"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/prometheus/common/version"
	"go.opencensus.io/zpages"
)

// Server contains the state of the main mtail program.
type Server struct {
	ctx    context.Context
	cancel context.CancelFunc
	store  *metrics.Store // Metrics storage
	w      watcher.Watcher
	wg     sync.WaitGroup // wait for main processes to shutdown

	t *tailer.Tailer     // t tails the watched files and sends lines to the VMs
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

	overrideLocation            *time.Location // Timezone location to use when parsing timestamps
	expiredMetricGcTickInterval time.Duration  // Interval between expired metric removal runs
	staleLogGcTickInterval      time.Duration  // Interval between stale log gc runs
	logPatternPollTickInterval  time.Duration  // Interval between log pattern polls
	metricPushInterval          time.Duration  // Interval between metric pushes
	syslogUseCurrentYear        bool           // if set, use the current year for timestamps that have no year information
	omitMetricSource            bool           // if set, do not link the source program to a metric
	omitProgLabel               bool           // if set, do not put the program name in the metric labels
	emitMetricTimestamp         bool           // if set, emit the metric's recorded timestamp
	omitDumpMetricsStore        bool           // if set, do not print the metric store; useful in test
}

// StartTailing adds each log path pattern to the tailer.
func (m *Server) StartTailing() error {
	var err error
	for _, pattern := range m.logPathPatterns {
		glog.V(1).Infof("Tail pattern %q", pattern)
		if err = m.t.TailPattern(pattern); err != nil {
			glog.Warning(err)
		}
	}
	return nil
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
	if m.programPath == "" {
		return nil
	}
	if errs := m.l.LoadAllPrograms(); errs != nil {
		return errors.Errorf("Compile encountered errors:\n%s", errs)
	}
	return nil
}

// initExporter sets up an Exporter for this Server.
func (m *Server) initExporter() (err error) {
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

// initTailer sets up a Tailer for this Server.
func (m *Server) initTailer() (err error) {
	opts := []tailer.Option{
		tailer.LogPatternPollTickInterval(m.logPatternPollTickInterval),
		tailer.StaleLogGcTickInterval(m.staleLogGcTickInterval),
	}
	if m.oneShot {
		opts = append(opts, tailer.OneShot)
	}
	if m.ignoreRegexPattern != "" {
		opts = append(opts, tailer.IgnoreRegex(m.ignoreRegexPattern))
	}
	if len(m.logPathPatterns) > 0 {
		opts = append(opts, tailer.LogPatterns(m.logPathPatterns))
	}
	m.t, err = tailer.New(m.ctx, &m.wg, m.lines, m.w, opts...)
	return
}

// New creates a MtailServer from the supplied Options.
func New(ctx context.Context, store *metrics.Store, w watcher.Watcher, options ...Option) (*Server, error) {
	m := &Server{
		store: store,
		w:     w,
		lines: make(chan *logline.LogLine),
		// Using a non-pedantic registry means we can be looser with metrics that
		// are not fully specified at startup.
		reg: prometheus.NewRegistry(),
	}
	m.ctx, m.cancel = context.WithCancel(ctx)

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

// WriteMetrics dumps the current state of the metrics store in JSON format to
// the io.Writer.
func (m *Server) WriteMetrics(w io.Writer) error {
	m.store.RLock()
	b, err := json.MarshalIndent(m.store.Metrics, "", "  ")
	m.store.RUnlock()
	if err != nil {
		return errors.Wrap(err, "failed to marshal metrics into json")
	}
	_, err = w.Write(b)
	return err
}

// Serve begins the webserver and awaits a shutdown instruction.
func (m *Server) Serve() error {
	mux := http.NewServeMux()
	mux.HandleFunc("/favicon.ico", FaviconHandler)
	mux.Handle("/", m)
	mux.Handle("/progz", http.HandlerFunc(m.l.ProgzHandler))
	mux.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	mux.Handle("/metrics", promhttp.HandlerFor(m.reg, promhttp.HandlerOpts{}))
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

	errc := make(chan error, 1)
	go func() {
		glog.Infof("Listening on %s", m.listener.Addr())
		if err := srv.Serve(m.listener); err != nil && err != http.ErrServerClosed {
			errc <- err
		}
	}()

	select {
	case err := <-errc:
		return err
	case <-m.ctx.Done():
		glog.Info("Shutdown requested.")
		// TODO(jaq): This code makes the tests slow and flaky.  Why?
		// ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		// defer cancel()
		// srv.SetKeepAlivesEnabled(false)
		// if err := srv.Shutdown(ctx); err != nil {
		// 	return err
		// }
		return srv.Close()
	}

	return nil
}

// Run starts MtailServer's primary function, in which it watches the log files
// for changes and sends any new lines found to the virtual machines. If
// OneShot mode is enabled, it will exit.
func (m *Server) Run() error {
	defer func() {
		// TODO(jaq): Do we need this cancel func for test?
		m.cancel()
		m.wg.Wait()
	}()
	if m.compileOnly {
		glog.Info("compile-only is set, exiting")
		return nil
	}
	if err := m.StartTailing(); err != nil {
		return err
	}
	if m.oneShot {
		if m.omitDumpMetricsStore {
			glog.Info("Store dump disabled, exiting")
			return nil
		}
		fmt.Printf("Metrics store:")
		if err := m.WriteMetrics(os.Stdout); err != nil {
			return err
		}
		return nil
	}
	if err := m.Serve(); err != nil {
		return err
	}
	glog.Info("END OF LINE")
	return nil
}

func (m *Server) Addr() string {
	return m.listener.Addr().String()
}
