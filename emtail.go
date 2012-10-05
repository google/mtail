// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"unicode/utf8"

	_ "net/http/pprof"
)

var (
	port  *string = flag.String("port", "3903", "HTTP port to listen on.")
	logs  *string = flag.String("logs", "", "List of files to monitor.")
	progs *string = flag.String("progs", "", "Directory containing programs")

	one_shot *bool = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")
)


func OneShot(logfile string, lines chan string) error {
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
	return nil
}

func StartEmtail(lines chan string, pathnames []string) {
	t := NewTailer(lines)

	for _, pathname := range pathnames {
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
	b := bufio.NewWriter(w)
	for _, l := range c.lines {
		b.WriteString(l)
	}
}

func main() {
	flag.Parse()

	if *progs == "" {
		log.Fatalf("No emtail program directory specified; use -progs")
	}
	if *logs == "" {
		log.Fatalf("No logs specified to tail; use -logs")
	}

	fis, err := ioutil.ReadDir(*progs)
	if err != nil {
		log.Fatalf("Failed to list programs in %q: %s", *progs, err)
	}

	e := &engine{}
	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		if filepath.Ext(fi.Name()) != ".em" {
			continue
		}
		f, err := os.Open(fmt.Sprintf("%s/%s", *progs, fi.Name()))
		if err != nil {
			log.Printf("Failed to read program %q: %s\n", fi.Name(), err)
			continue
		}
		defer f.Close()
		v, errs := Compile(fi.Name(), f)
		if errs != nil {
			errors = 1
			for _, e := range errs {
				log.Print(e)
			}
			continue
		}
		e.addVm(v)
	}

	if *compile_only {
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
	go e.run(lines)

	if *one_shot {
		for _, pathname := range pathnames {
			err := OneShot(pathname, lines)
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
		StartEmtail(lines, pathnames)

		c := &console{}
		log.SetOutput(c)

		http.Handle("/", c)
		http.HandleFunc("/json", handleJson)
		http.HandleFunc("/csv", handleCsv)
		StartMetricPush()

		log.Fatal(http.ListenAndServe(":"+*port, nil))
	}
}
