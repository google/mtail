// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"bufio"
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"net/http"
	"os"
	"os/signal"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/exporter"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"
)

// MtailServer contains the state of the main program object.
type MtailServer struct {
	lines chan *tailer.LogLine // Channel of lines from tailer to VM engine.
	store *metrics.Store       // Metrics storage.

	t *tailer.Tailer     // t tails the watched files and feeds lines to the VMs.
	l *vm.Loader         // l loads programs and manages the VM lifecycle.
	e *exporter.Exporter // e manages the export of metrics from the store.

	webquit   chan struct{} // Channel to signal shutdown from web UI.
	closeOnce sync.Once     // Ensure shutdown happens only once.

	o Options // Options passed in at creation time.
}

// OneShot reads the contents of a log file into the lines channel from start to finish, terminating the program at the end.
func (m *MtailServer) OneShot(logfile string, print bool) (count int64, err error) {
	glog.Infof("Oneshot %q", logfile)
	l, err := os.Open(logfile)
	if err != nil {
		return 0, fmt.Errorf("failed to open log file %q: %s", logfile, err)
	}
	defer l.Close()

	r := bufio.NewReader(l)

	if print {
		fmt.Printf("%s: %d MAXPROCS, %d CPUs, ", logfile, runtime.GOMAXPROCS(-1), runtime.NumCPU())
	}

	start := time.Now()

Loop:
	for {
		line, err := r.ReadString('\n')
		line = strings.TrimSuffix(line, "\n")
		switch {
		case err == io.EOF:
			if len(line) > 0 {
				m.lines <- tailer.NewLogLine(logfile, line)
			}
			break Loop
		case err != nil:
			return 0, fmt.Errorf("failed to read from %q: %s", logfile, err)
		default:
			m.lines <- tailer.NewLogLine(logfile, line)
		}
	}
	duration := time.Since(start)
	count, err = strconv.ParseInt(vm.LineCount.String(), 10, 64)
	if err != nil {
		return
	}
	if print {
		µsPerL := float64(duration.Nanoseconds()) / (float64(count) * 1000)
		fmt.Printf("%d lines, %s total time, %6.3f µs/line\n", count, duration, µsPerL)
	}
	return
}

// StartTailing constructs a new Tailer and commences sending log lines into
// the lines channel.
func (m *MtailServer) StartTailing() error {
	o := tailer.Options{Lines: m.lines, W: m.o.W, FS: m.o.FS}
	var err error
	m.t, err = tailer.New(o)
	if err != nil {
		return fmt.Errorf("couldn't create a log tailer: %s", err)
	}

	for _, pattern := range m.o.LogPathPatterns {
		glog.V(1).Infof("Tail pattern %q", pattern)
		if err = m.t.Tail(pattern); err != nil {
			glog.Error(err)
		}
	}
	for _, fd := range m.o.LogFds {
		f := os.NewFile(uintptr(fd), strconv.Itoa(fd))
		if f == nil {
			glog.Errorf("Attempt to reopen fd %q returned nil", fd)
			continue
		}
		if err = m.t.TailFile(f); err != nil {
			glog.Error(err)
		}
	}
	return nil
}

// InitLoader constructs a new program loader and performs the inital load of program files in the program directory.
func (m *MtailServer) InitLoader() error {
	o := vm.LoaderOptions{
		Store:                m.store,
		Lines:                m.lines,
		CompileOnly:          m.o.CompileOnly,
		DumpAst:              m.o.DumpAst,
		DumpAstTypes:         m.o.DumpAstTypes,
		DumpBytecode:         m.o.DumpBytecode,
		SyslogUseCurrentYear: m.o.SyslogUseCurrentYear,
		OmitMetricSource:     m.o.OmitMetricSource,
		W:                    m.o.W,
		FS:                   m.o.FS,
	}
	var err error
	m.l, err = vm.NewLoader(o)
	if err != nil {
		return err
	}
	if m.o.Progs != "" {
		errors := m.l.LoadProgs(m.o.Progs)
		if errors != nil {
			return fmt.Errorf("Compile encountered errors:\n%s", errors)
		}
	}
	return nil
}

const statusTemplate = `
<html>
<head>
<title>mtail on :{{.Port}}</title>
</head>
<body>
<h1>mtail on :{{.Port}}</h1>
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
		Port      string
		BuildInfo string
	}{
		m.o.Port,
		m.o.BuildInfo,
	}
	w.Header().Add("Content-type", "text/html")
	w.WriteHeader(http.StatusFound)
	if err := t.Execute(w, data); err != nil {
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

// Options contains all the parameters necessary for constructing a new MtailServer.
type Options struct {
	Progs                string
	LogPathPatterns      []string
	LogFds               []int
	Port                 string
	OneShot              bool
	OneShotMetrics       bool
	CompileOnly          bool
	DumpAst              bool
	DumpAstTypes         bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool
	OmitMetricSource     bool

	BuildInfo string

	Store *metrics.Store

	W  watcher.Watcher // Not required, will use watcher.LogWatcher if zero.
	FS afero.Fs        // Not required, will use afero.OsFs if zero.
}

// New creates an MtailServer from the supplied Options.
func New(o Options) (*MtailServer, error) {
	store := o.Store
	if store == nil {
		store = metrics.NewStore()
	}
	m := &MtailServer{
		lines:   make(chan *tailer.LogLine),
		store:   store,
		webquit: make(chan struct{}),
		o:       o}

	err := m.InitLoader()
	if err != nil {
		return nil, err
	}

	m.e, err = exporter.New(exporter.Options{Store: m.store})
	if err != nil {
		return nil, err
	}

	return m, nil
}

// WriteMetrics dumps the current state of the metrics store in JSON format to
// the io.Writer.
func (m *MtailServer) WriteMetrics(w io.Writer) error {
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
func (m *MtailServer) RunOneShot() {
	fmt.Println("Oneshot results:")
	for _, pathname := range m.o.LogPathPatterns {
		_, err := m.OneShot(pathname, true)
		if err != nil {
			glog.Exitf("Failed one shot mode for %q: %s\n", pathname, err)
		}
	}
	if m.o.OneShotMetrics {
		fmt.Printf("Metrics store:")
		if err := m.WriteMetrics(os.Stdout); err != nil {
			glog.Exit(err)
		}
	}
	m.Close()
}

// Serve begins the long-running mode of mtail, in which it watches the log
// files for changes and sends any new lines found into the lines channel for
// pick up by the virtual machines.  It will continue to do so until it is
// signalled to exit.
func (m *MtailServer) Serve() {
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
	m.WaitForShutdown()
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
func (m *MtailServer) Close() {
	m.closeOnce.Do(func() {
		glog.Info("Shutdown requested.")
		if m.t != nil {
			err := m.t.Close()
			if err != nil {
				glog.Infof("tailer close failed: %s", err)
			}
		} else {
			glog.Info("No tailer, closing lines channel.")
			close(m.lines)
		}
		if m.l != nil {
			<-m.l.VMsDone
		}
		glog.Info("All done.")
	})
}

// Run starts MtailServer in the configuration supplied in Options at creation.
func (m *MtailServer) Run() {
	if m.o.CompileOnly {
		return
	}
	if m.o.OneShot {
		m.RunOneShot()
	} else {
		m.Serve()
	}
}
