// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"context"
	"errors"
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
	"github.com/google/mtail/internal/runtime"
	"github.com/google/mtail/internal/tailer"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/collectors"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/prometheus/common/version"
	"go.opencensus.io/zpages"
)

// Server contains the state of the main mtail program.
type Server struct {
	ctx   context.Context
	store *metrics.Store // Metrics storage
	wg    sync.WaitGroup // wait for main processes to shutdown

	tOpts []tailer.Option    // options for constructing `t`
	t     *tailer.Tailer     // t manages log patterns and log streams, which sends lines to the VMs
	rOpts []runtime.Option   // options for constructing `r`
	r     *runtime.Runtime   // r loads programs and manages the VM lifecycle
	eOpts []exporter.Option  // options for constructing `e`
	e     *exporter.Exporter // e manages the export of metrics from the store

	lines chan *logline.LogLine // primary communication channel, owned by Tailer.

	reg *prometheus.Registry

	listener net.Listener // Configured with bind address.

	buildInfo BuildInfo // go build information

	programPath        string // path to programs to load
	oneShot            bool   // if set, mtail reads log files from the beginning, once, then exits
	compileOnly        bool   // if set, mtail compiles programs then exit
	httpDebugEndpoints bool   // if set, mtail will enable debug endpoints
	httpInfoEndpoints  bool   // if set, mtail will enable info endpoints for progz and varz
}

// initRuntime constructs a new runtime and performs the initial load of program files in the program directory.
func (m *Server) initRuntime() (err error) {
	m.r, err = runtime.New(m.lines, &m.wg, m.programPath, m.store, m.rOpts...)
	return
}

// initExporter sets up an Exporter for this Server.
func (m *Server) initExporter() (err error) {
	if m.oneShot {
		// This is a hack to avoid a race in test, but assume that in oneshot
		// mode we don't want to export anything.
		return nil
	}
	m.e, err = exporter.New(m.ctx, &m.wg, m.store, m.eOpts...)
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
	m.t, err = tailer.New(m.ctx, &m.wg, m.lines, m.tOpts...)
	return
}

// initHTTPServer begins the http server.
func (m *Server) initHTTPServer() error {
	initDone := make(chan struct{})
	defer close(initDone)

	if m.listener == nil {
		glog.Info("no listen address configured, not starting http server")
		return nil
	}

	mux := http.NewServeMux()
	if m.httpDebugEndpoints {
		mux.Handle("/debug/vars", expvar.Handler())
		mux.HandleFunc("/debug/pprof/", pprof.Index)
		mux.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)
		mux.HandleFunc("/debug/pprof/profile", pprof.Profile)
		mux.HandleFunc("/debug/pprof/symbol", pprof.Symbol)
		mux.HandleFunc("/debug/pprof/trace", pprof.Trace)
	}
	if m.httpInfoEndpoints {
		mux.HandleFunc("/favicon.ico", FaviconHandler)
		mux.HandleFunc("/varz", http.HandlerFunc(m.e.HandleVarz))
		mux.Handle("/progz", http.HandlerFunc(m.r.ProgzHandler))
	}
	mux.Handle("/", m)
	mux.Handle("/metrics", promhttp.HandlerFor(m.reg, promhttp.HandlerOpts{}))
	mux.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	mux.HandleFunc("/graphite", http.HandlerFunc(m.e.HandleGraphite))
	zpages.Handle(mux, "/")

	srv := &http.Server{
		ReadTimeout:       1 * time.Second,
		WriteTimeout:      1 * time.Second,
		IdleTimeout:       30 * time.Second,
		ReadHeaderTimeout: 2 * time.Second,
		Handler:           mux,
	}

	var wg sync.WaitGroup
	errc := make(chan error, 1)

	// This goroutine runs the http server.
	wg.Add(1)
	go func() {
		defer wg.Done()
		<-initDone
		glog.Infof("Listening on %s", m.listener.Addr())
		if err := srv.Serve(m.listener); err != nil && !errors.Is(err, http.ErrServerClosed) {
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
	m.rOpts = append(m.rOpts, runtime.PrometheusRegisterer(m.reg))

	// TODO(jaq): Should these move to initExporter?
	expvarDescs := map[string]*prometheus.Desc{
		// internal/exporter/prometheus.go
		 "metric_export_total":    prometheus.NewDesc("metric_export_total", "number of metrics exporting", nil, nil),
		// internal/tailer/file.go
		"log_errors_total":    prometheus.NewDesc("log_errors_total", "number of IO errors encountered per log file", []string{"logfile"}, nil),
		"log_rotations_total": prometheus.NewDesc("log_rotations_total", "number of log rotation events per log file", []string{"logfile"}, nil),
		"log_truncates_total": prometheus.NewDesc("log_truncates_total", "number of log truncation events log file", []string{"logfile"}, nil),
		"log_lines_total":     prometheus.NewDesc("log_lines_total", "number of lines read per log file", []string{"logfile"}, nil),
		// internal/runtime/loader.go
		"lines_total":               prometheus.NewDesc("lines_total", "number of lines received by the program loader", nil, nil),
		"prog_loads_total":          prometheus.NewDesc("prog_loads_total", "number of program load events by program source filename", []string{"prog"}, nil),
		"prog_load_errors_total":    prometheus.NewDesc("prog_load_errors_total", "number of errors encountered when loading per program source filename", []string{"prog"}, nil),
		"prog_runtime_errors_total": prometheus.NewDesc("prog_runtime_errors_total", "number of errors encountered when executing programs per source filename", []string{"prog"}, nil),
	}
	m.reg.MustRegister(
		collectors.NewGoCollector(),
		collectors.NewProcessCollector(collectors.ProcessCollectorOpts{}))
	// Prefix all expvar metrics with 'mtail_'
	prometheus.WrapRegistererWithPrefix("mtail_", m.reg).MustRegister(
		collectors.NewExpvarCollector(expvarDescs))
	if err := m.SetOption(options...); err != nil {
		return nil, err
	}
	if err := m.initExporter(); err != nil {
		return nil, err
	}
	//nolint:contextcheck // TODO
	if err := m.initRuntime(); err != nil {
		return nil, err
	}
	if err := m.initTailer(); err != nil {
		return nil, err
	}
	//nolint:contextcheck // TODO
	if err := m.initHTTPServer(); err != nil {
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
