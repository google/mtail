// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"sync"
	"syscall"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/exporter"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/pkg/errors"
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

// StartTailing constructs a new Tailer and commences sending log lines into
// the lines channel.
func (m *MtailServer) StartTailing() error {
	o := tailer.Options{Lines: m.lines, OneShot: m.o.OneShot, W: m.o.W, FS: m.o.FS}
	var err error
	m.t, err = tailer.New(o)
	if err != nil {
		return errors.Wrap(err, "couldn't create a log tailer")
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
		ErrorsAbort:          m.o.CompileOnly || m.o.OneShot,
		DumpAst:              m.o.DumpAst,
		DumpAstTypes:         m.o.DumpAstTypes,
		DumpBytecode:         m.o.DumpBytecode,
		SyslogUseCurrentYear: m.o.SyslogUseCurrentYear,
		OverrideLocation:     m.o.OverrideLocation,
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
		errs := m.l.LoadProgs(m.o.Progs)
		if errs != nil {
			return errors.Errorf("Compile encountered errors:\n%s", errs)
		}
	}
	return nil
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
		m.o.BindAddress,
		m.o.BuildInfo,
	}
	w.Header().Add("Content-type", "text/html")
	w.WriteHeader(http.StatusFound)
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

// Options contains all the parameters necessary for constructing a new MtailServer.
type Options struct {
	Progs                string
	LogPathPatterns      []string
	LogFds               []int
	BindAddress          string
	OneShot              bool
	CompileOnly          bool
	DumpAst              bool
	DumpAstTypes         bool
	DumpBytecode         bool
	SyslogUseCurrentYear bool
	OverrideLocation     *time.Location
	OmitMetricSource     bool
	OmitProgLabel        bool

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

	m.e, err = exporter.New(exporter.Options{Store: m.store, OmitProgLabel: o.OmitProgLabel})
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
		return errors.Wrap(err, "failed to marshal metrics into json")
	}
	w.Write(b)
	return nil
}

// Serve begins the webserver and awaits a shutdown instruction.
func (m *MtailServer) Serve() {
	http.Handle("/", m)
	http.HandleFunc("/json", http.HandlerFunc(m.e.HandleJSON))
	http.HandleFunc("/metrics", http.HandlerFunc(m.e.HandlePrometheusMetrics))
	http.HandleFunc("/varz", http.HandlerFunc(m.e.HandleVarz))
	http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
	m.e.StartMetricPush()

	go func() {
		glog.Infof("Listening on port %s", m.o.BindAddress)
		err := http.ListenAndServe(m.o.BindAddress, nil)
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
	n := make(chan os.Signal, 1)
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
func (m *MtailServer) Close() error {
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
	return nil
}

// Run starts MtailServer's primary function, in which it watches the log
// files for changes and sends any new lines found into the lines channel for
// pick up by the virtual machines. If OneShot mode is enabled, it will exit.
func (m *MtailServer) Run() error {
	if m.o.CompileOnly {
		return nil
	}
	err := m.StartTailing()
	if err != nil {
		glog.Exitf("tailing failed: %s", err)
	}
	if m.o.OneShot {
		err := m.Close()
		if err != nil {
			return err
		}
		fmt.Printf("Metrics store:")
		if err := m.WriteMetrics(os.Stdout); err != nil {
			return err
		}
	} else {
		m.Serve()
	}
	return nil
}
