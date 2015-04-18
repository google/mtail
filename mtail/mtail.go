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
	"strings"
	"sync"
	"syscall"

	"github.com/golang/glog"
	"github.com/google/mtail/exporter"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
)

// Mtail contains the state of the main program object.
type Mtail struct {
	lines chan string   // Channel of lines from tailer to VM engine.
	store metrics.Store // Metrics storage.

	pathnames []string // pathnames of logs to tail
	progs     string   // directory path containing mital programs to load
	port      string   // port to serve HTTP on

	t *tailer.Tailer     // t tails the watched files and feeds lines to the VMs.
	l *vm.Loader         // l loads programs and manages the VM lifecycle.
	e *exporter.Exporter // e manages the export of metrics from the store.

	webquit   chan struct{} // Channel to signal shutdown from web UI.
	closeOnce sync.Once     // Ensure shutdown happens only once.

	oneShot              bool
	compileOnly          bool
	dumpBytecode         bool
	syslogUseCurrentYear bool
}

// OneShot reads the contents of a log file into the lines channel from start to finish, terminating the program at the end.
func (m *Mtail) OneShot(logfile string, lines chan string) error {
	defer m.Close()
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
			lines <- line
		}
	}
}

// StartTailing constructs a new Tailer and commences sending log lines into
// the lines channel.
func (m *Mtail) StartTailing() {
	o := tailer.Options{Lines: m.lines}
	m.t = tailer.New(o)
	if m.t == nil {
		glog.Fatal("Couldn't create a log tailer.")
	}

	for _, pathname := range m.pathnames {
		m.t.Tail(pathname)
	}
}

// InitLoader constructs a new program loader and performs the inital load of program files in the program directory.
func (m *Mtail) InitLoader() {
	o := vm.LoaderOptions{Store: &m.store, Lines: m.lines, CompileOnly: m.compileOnly, DumpBytecode: m.dumpBytecode, SyslogUseCurrentYear: m.syslogUseCurrentYear}
	m.l = vm.NewLoader(o)
	if m.l == nil {
		glog.Fatal("Couldn't create a program loader.")
	}
	errors := m.l.LoadProgs(m.progs)
	if m.compileOnly || m.dumpBytecode {
		os.Exit(errors)
	}
}

func (m *Mtail) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte(`<a href="/json">json</a>, <a href="/metrics">prometheus metrics</a>`))
}

// NewMtail is a temporary function used for creating bare Mtail objects.
func NewMtail() *Mtail {
	return &Mtail{
		lines:   make(chan string),
		webquit: make(chan struct{}),
	}
}

// Options contains all the parameters necessary for constructing a new Mtail.
type Options struct {
	Progs                string
	Logs                 string
	Port                 string
	OneShot              bool
	CompileOnly          bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool
}

// New creates an Mtail from the supplied Options.
func New(o Options) *Mtail {
	if o.Progs == "" {
		glog.Fatalf("No mtail program directory specified; use -progs")
		return nil
	}
	if o.Logs == "" {
		glog.Fatalf("No logs specified to tail; use -logs")
		return nil
	}
	m := NewMtail()
	m.progs = o.Progs
	m.port = o.Port
	m.oneShot = o.OneShot
	m.compileOnly = o.CompileOnly
	m.dumpBytecode = o.DumpBytecode
	m.syslogUseCurrentYear = o.SyslogUseCurrentYear

	for _, pathname := range strings.Split(o.Logs, ",") {
		if pathname != "" {
			m.pathnames = append(m.pathnames, pathname)
		}
	}
	if len(m.pathnames) == 0 {
		glog.Fatal("No logs to tail.")
		return nil
	}

	m.InitLoader()

	m.e = exporter.New(exporter.Options{Store: &m.store})

	return m
}

// RunOneShot performs the work of the one_shot commandline flag; after compiling programs mtail will read all of the log files in full, once, dump the metric results at the end, and then exit.
func (m *Mtail) RunOneShot() {
	for _, pathname := range m.pathnames {
		err := m.OneShot(pathname, m.lines)
		if err != nil {
			glog.Fatalf("Failed one shot mode for %q: %s\n", pathname, err)
		}
	}
	b, err := json.MarshalIndent(m.store.Metrics, "", "  ")
	if err != nil {
		glog.Fatalf("Failed to marshal metrics into json: %s", err)
	}
	os.Stdout.Write(b)
	m.e.WriteMetrics()
}

// Serve begins the long-running mode of mtail, in which it watches the log
// files for changes and sends any new lines found into the lines channel for
// pick up by the virtual machines.  It will continue to do so until it is
// signalled to exit.
func (m *Mtail) Serve() {
	m.StartTailing()

	http.Handle("/", m)
	http.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	http.HandleFunc("/metrics", http.HandlerFunc(m.e.HandlePrometheusMetrics))
	http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
	m.e.StartMetricPush()

	go func() {
		glog.Infof("Listening on port %s", m.port)
		err := http.ListenAndServe(":"+m.port, nil)
		if err != nil {
			glog.Fatal(err)
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
	if m.oneShot {
		m.RunOneShot()
	} else {
		m.Serve()
	}
}
