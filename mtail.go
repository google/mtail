// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync"
	"syscall"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/jaqx0r/afero"

	_ "net/http/pprof"
)

var (
	port  *string = flag.String("port", "3903", "HTTP port to listen on.")
	logs  *string = flag.String("logs", "", "List of files to monitor.")
	progs *string = flag.String("progs", "", "Directory containing programs")

	one_shot *bool = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")
)

type mtail struct {
	lines chan string   // Channel of lines from tailer to VM engine.
	store metrics.Store // Metrics storage.

	t *tailer.Tailer // t tails the watched files and feeds lines to the VMs.
	l *vm.Loader     // l loads programs and manages the VM lifecycle.

	webquit   chan struct{} // Channel to signal shutdown from web UI.
	closeOnce sync.Once     // Ensure shutdown happens only once.
}

func (m *mtail) OneShot(logfile string, lines chan string) error {
	defer m.Close()
	l, err := os.Open(logfile)
	if err != nil {
		return fmt.Errorf("Failed to open log file %q: %s", logfile, err)
	}
	defer l.Close()

	r := bufio.NewReader(l)

	for {
		line, err := r.ReadString('\n')
		switch {
		case err == io.EOF:
			return nil
		case err != nil:
			return fmt.Errorf("Failed to read from %q: %s", logfile, err)
		default:
			lines <- line
		}
	}
}

func (m *mtail) StartTailing(pathnames []string) {
	w, err := watcher.NewLogWatcher()
	if err != nil {
		glog.Fatal("Couldn't create log path watcher:", err)
	}
	m.t = tailer.New(m.lines, w, &afero.OsFs{})
	if m.t == nil {
		glog.Fatal("Couldn't create a log tailer.")
	}

	for _, pathname := range pathnames {
		m.t.Tail(pathname)
	}
}

func (m *mtail) InitLoader(path string) {
	w, err := watcher.NewLogWatcher()
	if err != nil {
		glog.Fatal("Couldn't create prog watcher:", err)
	}
	m.l = vm.NewLoader(w, &m.store)
	if m.l == nil {
		glog.Fatal("Couldn't create a program loader.")
	}
	e, errors := m.l.LoadProgs(path)
	if *vm.Compile_only || *vm.Dump_bytecode {
		os.Exit(errors)
	}

	go e.Run(m.lines)

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

func (m *mtail) Serve() {
	if *progs == "" {
		glog.Fatalf("No mtail program directory specified; use -progs")
	}
	if *logs == "" {
		glog.Fatalf("No logs specified to tail; use -logs")
	}
	var pathnames []string
	for _, pathname := range strings.Split(*logs, ",") {
		if pathname != "" {
			pathnames = append(pathnames, pathname)
		}
	}
	if len(pathnames) == 0 {
		glog.Fatal("No logs to tail.")
	}

	m.InitLoader(*progs)

	ex := &Exporter{m.store}

	if *one_shot {
		for _, pathname := range pathnames {
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
		ex.WriteMetrics()
	} else {
		m.StartTailing(pathnames)

		http.Handle("/", m)
		http.HandleFunc("/json", http.HandlerFunc(ex.handleJson))
		http.HandleFunc("/metrics", http.HandlerFunc(ex.handlePrometheusMetrics))
		http.HandleFunc("/quitquitquit", http.HandlerFunc(m.handleQuit))
		ex.StartMetricPush()

		go func() {
			err := http.ListenAndServe(":"+*port, nil)
			if err != nil {
				glog.Fatal(err)
			}
		}()
		m.shutdownHandler()
	}
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
			close(m.lines)
		}
	})
}

func main() {
	flag.Parse()
	m := NewMtail()
	m.Serve()
}
