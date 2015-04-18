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

	_ "net/http/pprof"
)

type mtail struct {
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

func (m *mtail) OneShot(logfile string, lines chan string) error {
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

func (m *mtail) StartTailing() {
	o := tailer.Options{Lines: m.lines}
	m.t = tailer.New(o)
	if m.t == nil {
		glog.Fatal("Couldn't create a log tailer.")
	}

	for _, pathname := range m.pathnames {
		m.t.Tail(pathname)
	}
}

func (m *mtail) InitLoader() {
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

func (m *mtail) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte(`<a href="/json">json</a>, <a href="/metrics">prometheus metrics</a>`))
}

func NewMtail() *mtail {
	return &mtail{
		lines:   make(chan string),
		webquit: make(chan struct{}),
	}
}

type Options struct {
	Progs                string
	Logs                 string
	Port                 string
	OneShot              bool
	CompileOnly          bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool
}

func New(o Options) *mtail {
	if o.Progs == "" {
		glog.Fatalf("No mtail program directory specified; use -progs")
		return nil
	}
	if o.Logs == "" {
		glog.Fatalf("No logs specified to tail; use -logs")
		return nil
	}
	m := NewMtail()
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

func (m *mtail) RunOneShot() {
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

func (m *mtail) Serve() {
	m.StartTailing()

	http.Handle("/", m)
	http.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	http.HandleFunc("/metrics", http.HandlerFunc(m.e.HandlePrometheusMetrics))
	http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
	m.e.StartMetricPush()

	go func() {
		err := http.ListenAndServe(":"+m.port, nil)
		if err != nil {
			glog.Fatal(err)
		}
	}()
	m.shutdownHandler()
}

func (m *mtail) handleQuit(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		w.Header().Add("Allow", "POST")
		w.WriteHeader(http.StatusMethodNotAllowed)
		return
	}
	fmt.Fprintf(w, "Exiting...")
	close(m.webquit)
}

// shutdownHandler handles external shutdown request events.
func (m *mtail) shutdownHandler() {
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
func (m *mtail) Close() {
	m.closeOnce.Do(func() {
		glog.Info("Shutdown requested.")
		if m.t != nil {
			m.t.Close()
		} else {
			glog.Info("Closing lines")
			close(m.lines)
		}
		if m.l != nil {
			<-m.l.VMsDone
		}
	})
}

func (m *mtail) Run() {
	if m.oneShot {
		m.RunOneShot()
	} else {
		m.Serve()
	}
}
