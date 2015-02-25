// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Build the parser:
//go:generate go tool yacc -v y.output -o parser.go -p Mtail parser.y

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
	"strings"
	"unicode/utf8"

	_ "net/http/pprof"
)

var (
	port  *string = flag.String("port", "3903", "HTTP port to listen on.")
	logs  *string = flag.String("logs", "", "List of files to monitor.")
	progs *string = flag.String("progs", "", "Directory containing programs")

	one_shot      *bool = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")
	dump_bytecode *bool = flag.Bool("dump_bytecode", false, "Dump bytecode of programs and exit.")
)

func OneShot(logfile string, lines chan string, stop chan bool) error {
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

func StartTailing(lines chan string, pathnames []string) {
	tw, err := NewInotifyWatcher()
	if err != nil {
		log.Fatal("Couldn't create log path watcher:", err)
	}
	t := NewTailer(lines, tw)
	if t == nil {
		log.Fatal("Couldn't create a log tailer.")
	}

	for _, pathname := range pathnames {
		log.Printf("tailling %s\n", pathname)
		t.Tail(pathname)
	}
}

type console struct {
	lines []string
}

func (c *console) Write(p []byte) (n int, err error) {
	s := ""
	for i, width := 0, 0; i < len(p); i += width {
		var r rune
		r, width = utf8.DecodeRune(p[i:])
		s += string(r)
	}
	c.lines = append(c.lines, s)
	return len(s), nil
}

func (c *console) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(200)
	w.Write([]byte(`<a href="/json">json</a>, <a href="/metrics">prometheus metrics</a>`))
	w.Write([]byte("<pre>"))
	for _, l := range c.lines {
		w.Write([]byte(l))
	}
	w.Write([]byte("</pre>"))
}

func main() {
	flag.Parse()

	if *progs == "" {
		log.Fatalf("No mtail program directory specified; use -progs")
	}
	if *logs == "" {
		log.Fatalf("No logs specified to tail; use -logs")
	}

	w, err := NewInotifyWatcher()
	if err != nil {
		log.Fatal("Couldn't create an inotify watcher:", err)
	}

	p := NewProgLoader(w)
	if p == nil {
		log.Fatal("Couldn't create a program loader.")
	}
	e, errors := p.LoadProgs(*progs)

	if *compile_only || *dump_bytecode {
		os.Exit(errors)
	}

	var pathnames []string
	for _, pathname := range strings.Split(*logs, ",") {
		if pathname != "" {
			pathnames = append(pathnames, pathname)
		}
	}
	if len(pathnames) == 0 {
		log.Fatal("No logs to tail.")
	}

	lines := make(chan string)
	stop := make(chan bool, 1)
	go e.run(lines, stop)

	if *one_shot {
		for _, pathname := range pathnames {
			err := OneShot(pathname, lines, stop)
			if err != nil {
				log.Fatalf("Failed one shot mode for %q: %s\n", pathname, err)
			}
		}
		b, err := json.MarshalIndent(metrics, "", "  ")
		if err != nil {
			log.Fatalf("Failed to marshal metrics into json: %s", err)
		}
		os.Stdout.Write(b)
		WriteMetrics()
	} else {
		StartTailing(lines, pathnames)

		c := &console{}
		log.SetOutput(c)

		http.Handle("/", c)
		http.HandleFunc("/json", handleJson)
		http.HandleFunc("/metrics", handlePrometheusMetrics)
		StartMetricPush()

		log.Fatal(http.ListenAndServe(":"+*port, nil))
	}
}
