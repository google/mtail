// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync"
	"syscall"

	"github.com/golang/glog"
	"github.com/google/mtail/compiler"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/tailer"
	"github.com/google/mtail/watcher"
	"github.com/spf13/afero"

	_ "net/http/pprof"
)

var (
	port  *string = flag.String("port", "3903", "HTTP port to listen on.")
	logs  *string = flag.String("logs", "", "List of files to monitor.")
	progs *string = flag.String("progs", "", "Directory containing programs")

	one_shot      *bool = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")
	dump_bytecode *bool = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")
)

type mtail struct {
	lines chan string
	stop  chan bool

	closeOnce sync.Once
}

var (
	store metrics.Store
)

func (m *mtail) OneShot(logfile string, lines chan string, stop chan bool) error {
	defer func() { stop <- true }()
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
	tw, err := watcher.NewLogWatcher()
	if err != nil {
		glog.Fatal("Couldn't create log path watcher:", err)
	}
	t := tailer.New(m.lines, tw, &afero.OsFs{})
	if t == nil {
		glog.Fatal("Couldn't create a log tailer.")
	}

	for _, pathname := range pathnames {
		t.Tail(pathname)
	}
}

func (m *mtail) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte(`<a href="/json">json</a>, <a href="/metrics">prometheus metrics</a>`))
}

func NewMtail() *mtail {
	return &mtail{
		lines: make(chan string),
		stop:  make(chan bool, 1),
	}

}

func (m *mtail) Serve() {

	if *progs == "" {
		glog.Fatalf("No mtail program directory specified; use -progs")
	}
	if *logs == "" {
		glog.Fatalf("No logs specified to tail; use -logs")
	}

	w, err := watcher.NewLogWatcher()
	if err != nil {
		glog.Fatal("Couldn't create an inotify watcher:", err)
	}

	p := NewProgLoader(w)
	if p == nil {
		glog.Fatal("Couldn't create a program loader.")
	}
	e, errors := p.LoadProgs(*progs)

	if *compiler.Compile_only || *compiler.Dump_bytecode {
		os.Exit(errors)
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

	go e.run(m.lines, m.stop)

	if *one_shot {
		for _, pathname := range pathnames {
			err := m.OneShot(pathname, m.lines, m.stop)
			if err != nil {
				glog.Fatalf("Failed one shot mode for %q: %s\n", pathname, err)
			}
		}
		b, err := json.MarshalIndent(metrics, "", "  ")
		if err != nil {
			glog.Fatalf("Failed to marshal metrics into json: %s", err)
		}
		os.Stdout.Write(b)
		WriteMetrics()
	} else {
		m.StartTailing(pathnames)

		http.Handle("/", m)
		http.HandleFunc("/json", handleJson)
		http.HandleFunc("/metrics", handlePrometheusMetrics)
		StartMetricPush()

		go func() {
			err := http.ListenAndServe(":"+*port, nil)
			if err != nil {
				glog.Fatal(err)
			}
		}()
		m.interruptHandler()
	}
}

func (m *mtail) interruptHandler() {
	n := make(chan os.Signal)
	signal.Notify(n, os.Interrupt, syscall.SIGTERM)
	<-n
	log.Print("Received SIGTERM, exiting...")
	m.Close()
}

func (m *mtail) Close() {
	m.closeOnce.Do(m.close)
}

func (m *mtail) close() {
	log.Print("Shutdown requested.")
	close(m.lines)

}

func main() {
	store.ClearMetrics()
	flag.Parse()
	m := NewMtail()
	m.Serve()
}
