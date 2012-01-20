// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"

	_ "net/http/pprof"
)

var port *string = flag.String("port", "3903", "HTTP port to listen on.")
var logs *string = flag.String("logs", "", "List of files to monitor.")
var progs *string = flag.String("progs", "", "Directory containing programs")
var compile_only *bool = flag.Bool("compile_only", false, "Compile programs only.")

// Global metrics storage.
var (
	metric_lock sync.RWMutex
	metrics     []*Metric
)

// CSV export
func handleCsv(w http.ResponseWriter, r *http.Request) {
	c := csv.NewWriter(w)
	for _, m := range metrics {
		record := []string{m.Name,
			fmt.Sprintf("%s", m.Value),
			fmt.Sprintf("%s", m.Time),
			fmt.Sprintf("%d", m.Type),
			m.Unit}
		for k, v := range m.Tags {
			record = append(record, fmt.Sprintf("%s=%s", k, v))
		}
		c.Write(record)
	}
	c.Flush()
}

// JSON export
func handleJson(w http.ResponseWriter, r *http.Request) {
	b, err := json.MarshalForHTML(metrics)
	if err != nil {
		log.Println("error marshalling metrics into json:", err.Error())
	}
	w.Write(b)
}

// vms contains a list of virtual machines to execute when each new line is received
var vms []*vm

// RunVms receives a line from a channel and sends it to all VMs.
func RunVms(lines chan string) {
	for {
		select {
		case line := <-lines:
			for _, v := range vms {
				go v.Run(line)
			}
		}
	}
}

func main() {
	flag.Parse()
	w := NewWatcher()
	t := NewTailer(w)

	fis, err := ioutil.ReadDir(*progs)
	if err != nil {
		log.Fatalf("Failure reading progs from %q: %s", *progs, err)
	}

	errors := 0
	for _, fi := range fis {
		if fi.IsDir() {
			continue
		}
		if filepath.Ext(fi.Name()) != "em" {
			continue
		}
		f, err := os.Open(fmt.Sprintf("%s/%s", *progs, fi.Name()))
		if err != nil {
			log.Printf("Failed to open %s: %s\n", fi.Name(), err)
			continue
		}
		defer f.Close()
		vm, errs := Compile(fi.Name(), f)
		if errs != nil {
			errors = 1
			for _, e := range errs {
				log.Printf(e)
			}
			continue
		}
		vms = append(vms, vm)
	}

	if *compile_only {
		os.Exit(errors)
	}

	go RunVms(t.Line)
	go t.start()
	go w.start()

	for _, pathname := range strings.Split(*logs, ",") {
		t.Tail(pathname)
	}

	http.HandleFunc("/json", handleJson)
	http.HandleFunc("/csv", handleCsv)
	log.Fatal(http.ListenAndServe(":"+*port, nil))

}
