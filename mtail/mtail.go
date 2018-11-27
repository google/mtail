// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/exporter"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/pkg/errors"
	"github.com/spf13/afero"
)

// MtailServer contains the state of the main program object.
type MtailServer struct {
	lines chan *logline.LogLine // Channel of lines from tailer to VM engine.
	store *metrics.Store        // Metrics storage.
	w     watcher.Watcher
	fs    afero.Fs

	t *tailer.Tailer     // t tails the watched files and feeds lines to the VMs.
	l *vm.MasterControl  // l loads programs and manages the VM lifecycle.
	e *exporter.Exporter // e manages the export of metrics from the store.

	webquit   chan struct{} // Channel to signal shutdown from web UI.
	closeOnce sync.Once     // Ensure shutdown happens only once.

	overrideLocation *time.Location // Timezone location to use when parsing timestamps
	pollInterval     time.Duration  // Interval between polls of the filesystem
	bindAddress      string         // address to bind HTTP server
	buildInfo        string         // go build information
	programPath      string         // path to programs to load
	logPathPatterns  []string       // list of patterns to watch for log files to tail

	oneShot      bool // if set, mtail reads log files from the beginning, once, then exits
	compileOnly  bool // if set, mtail compiles programs then exits
	dumpAst      bool // if set, mtail prints the program syntax tree after parse
	dumpAstTypes bool // if set, mtail prints the program syntax tree after type checking
	dumpBytecode bool // if set, mtail prints the program bytecode after code generation

	syslogUseCurrentYear bool // if set, use the current year for timestamps that have no year information
	omitMetricSource     bool // if set, do not link the source program to a metric
	omitProgLabel        bool // if set, do not put the program name in the metric labels
}

// StartTailing constructs a new Tailer and commences sending log lines into
// the lines channel.
func (m *MtailServer) StartTailing() error {
	var err error
	for _, pattern := range m.logPathPatterns {
		glog.V(1).Infof("Tail pattern %q", pattern)
		if err = m.t.TailPattern(pattern); err != nil {
			glog.Error(err)
		}
	}
	return nil
}

// initLoader constructs a new program loader and performs the initial load of program files in the program directory.
func (m *MtailServer) initLoader() error {
	opts := []func(*vm.MasterControl) error{}
	if m.compileOnly {
		opts = append(opts, vm.CompileOnly)
		if m.oneShot {
			opts = append(opts, vm.ErrorsAbort)
		}
	}
	if m.dumpAst {
		opts = append(opts, vm.DumpAst)
	}
	if m.dumpAstTypes {
		opts = append(opts, vm.DumpAstTypes)
	}
	if m.dumpBytecode {
		opts = append(opts, vm.DumpBytecode)
	}
	if m.syslogUseCurrentYear {
		opts = append(opts, vm.SyslogUseCurrentYear)
	}
	if m.omitMetricSource {
		opts = append(opts, vm.OmitMetricSource)
	}
	if m.overrideLocation != nil {
		opts = append(opts, vm.OverrideLocation(m.overrideLocation))
	}
	var err error
	m.l, err = vm.NewLoader(m.programPath, m.store, m.lines, m.w, m.fs, opts...)
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

// initExporter sets up an Exporter for this MtailServer.
func (m *MtailServer) initExporter() (err error) {
	opts := []func(*exporter.Exporter) error{}
	if m.omitProgLabel {
		opts = append(opts, exporter.OmitProgLabel)
	}
	m.e, err = exporter.New(m.store, opts...)
	return
}

// initTailer sets up a Tailer for this MtailServer.
func (m *MtailServer) initTailer() (err error) {
	opts := []func(*tailer.Tailer) error{
		tailer.PollInterval(m.pollInterval),
	}
	if m.oneShot {
		opts = append(opts, tailer.OneShot)
	}
	m.t, err = tailer.New(m.lines, m.fs, m.w, opts...)
	return
}

const statusTemplate = `
<html>
<head>
<title>mtail on {{.BindAddress}}</title>
</head>
<body>
<h1>mtail on {{.BindAddress}}</h1>
<p>Build: {{.BuildInfo}}</p>
<p>Metrics: <a href="/json">json</a>, <a href="/metrics">prometheus</a>, <a href="/varz">varz</a></p>
<p>Debug: <a href="/debug/pprof">debug/pprof</a>, <a href="/debug/vars">debug/vars</a></p>
`

func (m *MtailServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	t, err := template.New("status").Parse(statusTemplate)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	data := struct {
		BindAddress string
		BuildInfo   string
	}{
		m.bindAddress,
		m.buildInfo,
	}
	w.Header().Add("Content-type", "text/html")
	w.WriteHeader(http.StatusOK)
	if err = t.Execute(w, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	err = m.l.WriteStatusHTML(w)
	if err != nil {
		glog.Warningf("Error while writing loader status: %s", err)
	}
	err = m.t.WriteStatusHTML(w)
	if err != nil {
		glog.Warningf("Error while writing tailer status: %s", err)
	}
}

// ProgramPath sets the path to find mtail programs in the MtailServer.
func ProgramPath(path string) func(*MtailServer) error {
	return func(m *MtailServer) error {
		m.programPath = path
		return nil
	}
}

// LogPathPatterns sets the patterns to find log paths in the MtailServer.
func LogPathPatterns(patterns ...string) func(*MtailServer) error {
	return func(m *MtailServer) error {
		m.logPathPatterns = patterns
		return nil
	}
}

// BindAddress sets the HTTP server address in MtailServer.
func BindAddress(address, port string) func(*MtailServer) error {
	return func(m *MtailServer) error {
		m.bindAddress = net.JoinHostPort(address, port)
		return nil
	}
}

// BuildInfo sets the mtail program build information in the MtailServer.
func BuildInfo(info string) func(*MtailServer) error {
	return func(m *MtailServer) error {
		m.buildInfo = info
		return nil
	}
}

// OverrideLocation sets the timezone location for log timestamps without any such information.
func OverrideLocation(loc *time.Location) func(*MtailServer) error {
	return func(m *MtailServer) error {
		m.overrideLocation = loc
		return nil
	}
}

// PollInterval sets the polling interval to use on a Tailer.
func PollInterval(interval time.Duration) func(*MtailServer) error {
	return func(m *MtailServer) error {
		if interval < 0 {
			return errors.New("poll_interval must be positive, or zero to disable.")
		}
		m.pollInterval = interval
		return nil
	}
}

// OneShot sets one-shot mode in the MtailServer.
func OneShot(m *MtailServer) error {
	m.oneShot = true
	return nil
}

// CompileOnly sets compile-only mode in the MtailServer.
func CompileOnly(m *MtailServer) error {
	m.compileOnly = true
	return nil
}

// DumpAst instructs the MtailServer's compiler to print the AST after parsing.
func DumpAst(m *MtailServer) error {
	m.dumpAst = true
	return nil
}

// DumpAstTypes instructs the MtailServer's copmiler to print the AST after type checking.
func DumpAstTypes(m *MtailServer) error {
	m.dumpAstTypes = true
	return nil
}

// DumpBytecode instructs the MtailServer's compiuler to print the program bytecode after code generation.
func DumpBytecode(m *MtailServer) error {
	m.dumpBytecode = true
	return nil
}

// SyslogUseCurrentYear instructs the MtailServer to use the current year for year-less log timestamp during parsing.
func SyslogUseCurrentYear(m *MtailServer) error {
	m.syslogUseCurrentYear = true
	return nil
}

// OmitProgLabel sets the MtailServer to not put the program name as a label in exported metrics.
func OmitProgLabel(m *MtailServer) error {
	m.omitProgLabel = true
	return nil
}

// OmitMetricSource sets the MtailServer to not link created metrics to their source program.
func OmitMetricSource(m *MtailServer) error {
	m.omitMetricSource = true
	return nil
}

// New creates a MtailServer from the supplied Options.
func New(store *metrics.Store, w watcher.Watcher, fs afero.Fs, options ...func(*MtailServer) error) (*MtailServer, error) {
	m := &MtailServer{
		store:   store,
		lines:   make(chan *logline.LogLine),
		w:       w,
		fs:      fs,
		webquit: make(chan struct{}),
	}
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
func (m *MtailServer) SetOption(options ...func(*MtailServer) error) error {
	for _, option := range options {
		if err := option(m); err != nil {
			return err
		}
	}
	return nil
}

// WriteMetrics dumps the current state of the metrics store in JSON format to
// the io.Writer.
func (m *MtailServer) WriteMetrics(w io.Writer) error {
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
func (m *MtailServer) Serve() error {
	if m.bindAddress == "" {
		return errors.Errorf("No bind address provided.")
	}
	http.HandleFunc("/favicon.ico", faviconHandler)
	http.Handle("/", m)
	http.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	http.HandleFunc("/metrics", http.HandlerFunc(m.e.HandlePrometheusMetrics))
	http.HandleFunc("/varz", http.HandlerFunc(m.e.HandleVarz))
	http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
	m.e.StartMetricPush()

	go func() {
		glog.Infof("Listening on port %s", m.bindAddress)
		err := http.ListenAndServe(m.bindAddress, nil)
		if err != nil {
			glog.Exit(err)
		}
	}()
	m.WaitForShutdown()
	return nil
}

func (m *MtailServer) handleQuit(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		w.Header().Add("Allow", "POST")
		w.WriteHeader(http.StatusMethodNotAllowed)
		return
	}
	fmt.Fprintf(w, "Exiting...")
	close(m.webquit)
}

// WaitForShutdown handles shutdown requests from the system or the UI.
func (m *MtailServer) WaitForShutdown() {
	n := make(chan os.Signal, 1)
	signal.Notify(n, os.Interrupt, syscall.SIGTERM)
	select {
	case <-n:
		glog.Info("Received SIGTERM, exiting...")
	case <-m.webquit:
		glog.Info("Received Quit from HTTP, exiting...")
	}
	if err := m.Close(); err != nil {
		glog.Warning(err)
	}
}

// Close handles the graceful shutdown of this mtail instance, ensuring that it only occurs once.
func (m *MtailServer) Close() error {
	m.closeOnce.Do(func() {
		glog.Info("Shutdown requested.")
		// If we have a tailer (i.e. not in test) then signal the tailer to
		// shut down, which will cause the watcher to shut down and for the
		// lines channel to close, causing the loader to start shutdown.
		if m.t != nil {
			err := m.t.Close()
			if err != nil {
				glog.Infof("tailer close failed: %s", err)
			}
		} else {
			// Without a tailer, MtailServer has ownership of the lines channel.
			glog.V(2).Info("No tailer, closing lines channel directly.")
			close(m.lines)
		}
		// If we have a loader, wait for it to signal that it has completed shutdown.
		if m.l != nil {
			<-m.l.VMsDone
		} else {
			glog.V(2).Info("No loader, so not waiting for loader shutdown.")
		}
		glog.Info("All done.")
	})
	return nil
}

// Run starts MtailServer's primary function, in which it watches the log
// files for changes and sends any new lines found into the lines channel for
// pick up by the virtual machines. If OneShot mode is enabled, it will exit.
func (m *MtailServer) Run() error {
	if m.compileOnly {
		glog.Info("compile-only is set, exiting")
		return nil
	}
	if err := m.StartTailing(); err != nil {
		glog.Exitf("tailing failed: %s", err)
	}
	if m.oneShot {
		err := m.Close()
		if err != nil {
			return err
		}
		fmt.Printf("Metrics store:")
		if err := m.WriteMetrics(os.Stdout); err != nil {
			return err
		}
	} else {
		if err := m.Serve(); err != nil {
			return err
		}
	}
	m.store.StartExpiryLoop()
	return nil
}

func faviconHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "image/x-icon")
	w.Header().Set("Cache-Control", "public, max-age=7776000")
	if _, err := w.Write(logoFavicon); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
