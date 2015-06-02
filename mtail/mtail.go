// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"

	"github.com/golang/glog"
	"github.com/google/mtail/exporter"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"
)

// Mtail contains the state of the main program object.
type Mtail struct {
	lines chan string   // Channel of lines from tailer to VM engine.
	store metrics.Store // Metrics storage.

	t *tailer.Tailer     // t tails the watched files and feeds lines to the VMs.
	l *vm.Loader         // l loads programs and manages the VM lifecycle.
	e *exporter.Exporter // e manages the export of metrics from the store.

	webquit   chan struct{} // Channel to signal shutdown from web UI.
	closeOnce sync.Once     // Ensure shutdown happens only once.

	o Options // Options passed in at creation time.
}

// OneShot reads the contents of a log file into the lines channel from start to finish, terminating the program at the end.
func (m *Mtail) OneShot(logfile string) error {
	l, err := os.Open(logfile)
	if err != nil {
		return fmt.Errorf("failed to open log file %q: %s", logfile, err)
	}
	defer l.Close()

	r := bufio.NewReader(l)

	for {
		line, err := r.ReadString('\n')
		switch {
		case err == io.EOF:
			return nil
		case err != nil:
			return fmt.Errorf("failed to read from %q: %s", logfile, err)
		default:
			m.lines <- line
		}
	}
}

// StartTailing constructs a new Tailer and commences sending log lines into
// the lines channel.
func (m *Mtail) StartTailing() error {
	o := tailer.Options{Lines: m.lines, W: m.o.W, FS: m.o.FS}
	var err error
	m.t, err = tailer.New(o)
	if err != nil {
		return fmt.Errorf("couldn't create a log tailer: %s", err)
	}

	for _, pathname := range m.o.Logs {
		m.t.Tail(pathname)
	}
	return nil
}

// InitLoader constructs a new program loader and performs the inital load of program files in the program directory.
func (m *Mtail) InitLoader() error {
	o := vm.LoaderOptions{Store: &m.store, Lines: m.lines, CompileOnly: m.o.CompileOnly, DumpBytecode: m.o.DumpBytecode, SyslogUseCurrentYear: m.o.SyslogUseCurrentYear, W: m.o.W, FS: m.o.FS}
	var err error
	m.l, err = vm.NewLoader(o)
	if err != nil {
		return err
	}
	if m.o.Progs != "" {
		errors := m.l.LoadProgs(m.o.Progs)
		if m.o.CompileOnly || m.o.DumpBytecode {
			return fmt.Errorf("Compile encountered errors:\n%s", errors)
		}
	}
	return nil
}

func (m *Mtail) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte(`<a href="/json">json</a>, <a href="/metrics">prometheus metrics</a>, <a href="/varz">varz</a>`))
}

// Options contains all the parameters necessary for constructing a new Mtail.
type Options struct {
	Progs                string
	Logs                 []string
	Port                 string
	OneShot              bool
	CompileOnly          bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool

	W  watcher.Watcher // Not required, will use watcher.LogWatcher if zero.
	FS afero.Fs        // Not required, will use afero.OsFs if zero.
}

// New creates an Mtail from the supplied Options.
func New(o Options) (*Mtail, error) {
	m := &Mtail{
		lines:   make(chan string),
		webquit: make(chan struct{}),
		o:       o}

	err := m.InitLoader()
	if err != nil {
		return nil, err
	}

	m.e, err = exporter.New(exporter.Options{Store: &m.store})
	if err != nil {
		return nil, err
	}

	return m, nil
}

// WriteMetrics dumps the current state of the metrics store in JSON format to
// the io.Writer.
func (m *Mtail) WriteMetrics(w io.Writer) error {
	m.store.RLock()
	b, err := json.MarshalIndent(m.store.Metrics, "", "  ")
	m.store.RUnlock()
	if err != nil {
		return fmt.Errorf("failed to marshal metrics into json: %s", err)
	}
	w.Write(b)
	return nil
}

// RunOneShot performs the work of the one_shot commandline flag; after compiling programs mtail will read all of the log files in full, once, dump the metric results at the end, and then exit.
func (m *Mtail) RunOneShot() {
	for _, pathname := range m.o.Logs {
		err := m.OneShot(pathname)
		if err != nil {
			glog.Exitf("Failed one shot mode for %q: %s\n", pathname, err)
		}
	}
	if err := m.WriteMetrics(os.Stdout); err != nil {
		glog.Exit(err)
	}
	m.e.WriteMetrics()
	m.Close()
}

// Serve begins the long-running mode of mtail, in which it watches the log
// files for changes and sends any new lines found into the lines channel for
// pick up by the virtual machines.  It will continue to do so until it is
// signalled to exit.
func (m *Mtail) Serve() {
	err := m.StartTailing()
	if err != nil {
		glog.Exitf("tailing failed: %s", err)
	}

	http.Handle("/", m)
	http.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	http.HandleFunc("/metrics", http.HandlerFunc(m.e.HandlePrometheusMetrics))
	http.HandleFunc("/varz", http.HandlerFunc(m.e.HandleVarz))
	http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
	m.e.StartMetricPush()

	go func() {
		glog.Infof("Listening on port %s", m.o.Port)
		err := http.ListenAndServe(":"+m.o.Port, nil)
		if err != nil {
			glog.Exit(err)
		}
	}()
	m.shutdownHandler()
}

func (m *Mtail) handleQuit(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		w.Header().Add("Allow", "POST")
		w.WriteHeader(http.StatusMethodNotAllowed)
		return
	}
	fmt.Fprintf(w, "Exiting...")
	close(m.webquit)
}

// shutdownHandler handles external shutdown request events.
func (m *Mtail) shutdownHandler() {
	n := make(chan os.Signal)
	signal.Notify(n, os.Interrupt, syscall.SIGTERM)
	select {
	case <-n:
		glog.Info("Received SIGTERM, exiting...")
	case <-m.webquit:
		glog.Info("Received Quit from UI, exiting...")
	}
	m.Close()
}

// Close handles the graceful shutdown of this mtail instance, ensuring that it only occurs once.
func (m *Mtail) Close() {
	m.closeOnce.Do(func() {
		glog.Info("Shutdown requested.")
		if m.t != nil {
			m.t.Close()
		} else {
			glog.Info("Closing lines channel.")
			close(m.lines)
		}
		if m.l != nil {
			<-m.l.VMsDone
		}
		glog.Info("All done.")
	})
}

// Run starts Mtail in the configuration supplied in Options at creation.
func (m *Mtail) Run() {
	if m.o.OneShot {
		m.RunOneShot()
	} else {
		m.Serve()
	}
}
