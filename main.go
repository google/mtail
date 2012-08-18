// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"expvar"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	_ "net/http/pprof"
)

var (
	port  *string = flag.String("port", "3903", "HTTP port to listen on.")
	logs  *string = flag.String("logs", "", "List of files to monitor.")
	progs *string = flag.String("progs", "", "Directory containing programs")

	one_shot *bool = flag.Bool("one_shot", false, "Run once on a log file, dump json, and exit.")

	collectd_socketpath *string = flag.String("collectd_socketpath", "",
		"Path to collectd unixsock to write metrics to.")
	graphite_hostport *string = flag.String("graphite_hostport", "",
		"Host:port to graphite carbon server to write metrics to.")
	statsd_hostport *string = flag.String("statsd_hostport", "",
		"Host:port to statsd server to write metrics to.")
	push_interval *int = flag.Int("metric_push_interval_seconds", 60,
		"Interval between metric pushes, in seconds")
)

var (
	line_count = expvar.NewInt("line_count")
	log_count  = expvar.NewInt("log_count")
)

var (
	last_metric_push_time time.Time
)

// RunVms receives a line from a channel and sends it to all VMs.
func RunVms(vms []*vm, lines chan string) {
	for {
		select {
		case line := <-lines:
			line_count.Add(1)
			for _, v := range vms {
				go v.Run(line)
			}
		}
	}
}

// vms contains a list of virtual machines to execute when each new line is received
var (
	vms []*vm
)

func OneShot(logfile string, lines chan string) error {
	l, err := os.Open(logfile)
	if err != nil {
		return fmt.Errorf("%s: could not open log file: %s", logfile, err)
	}
	defer l.Close()

	r := bufio.NewReader(l)

	for {
		line, err := r.ReadString('\n')
		switch {
		case err == io.EOF:
			return nil
		case err != nil:
			return fmt.Errorf("%s: read error: %s", logfile, err)
		default:
			lines <- line
		}
	}
	return nil
}

func WriteMetrics() {
	if metric_update_time.Sub(last_metric_push_time) <= 0 {
		return
	}
	if *collectd_socketpath != "" {
		err := CollectdWriteMetrics(*collectd_socketpath)
		if err != nil {
			log.Printf("collectd write error: %s\n", err)
		}
	}
	if *graphite_hostport != "" {
		err := GraphiteWriteMetrics(*graphite_hostport)
		if err != nil {
			log.Printf("graphite write error: %s\n", err)
		}
	}
	if *statsd_hostport != "" {
		err := StatsdWriteMetrics(*statsd_hostport)
		if err != nil {
			log.Printf("statsd error: %s\n", err)
		}
	}
	last_metric_push_time = time.Now()
}

func main() {
	flag.Parse()

	if *progs == "" {
		log.Fatalf("No progs directory specified; use -progs")
	}
	if *logs == "" {
		log.Fatalf("No logs specified to tail; use -logs")
	}

	fis, err := ioutil.ReadDir(*progs)
	if err != nil {
		log.Fatalf("Failure reading progs from %q: %s", *progs, err)
	}

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
			log.Printf("Failed to open %s: %s\n", fi.Name(), err)
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
		vms = append(vms, v)
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
	go RunVms(vms, lines)

	if *one_shot {
		for _, pathname := range pathnames {
			err := OneShot(pathname, lines)
			if err != nil {
				log.Fatalf("Error in one shot mode: %s\n", err)
			}
		}
		b, err := json.MarshalIndent(metrics, "", "  ")
		if err != nil {
			log.Fatalf("Error marshalling metrics into json: ", err.Error())
		}
		os.Stdout.Write(b)
		WriteMetrics()
	} else {
		w := NewWatcher()
		t := NewTailer(w, lines)

		for _, pathname := range pathnames {
			if t.Tail(pathname) {
				log_count.Add(1)
			}
		}

		http.HandleFunc("/json", handleJson)
		http.HandleFunc("/csv", handleCsv)
		if *collectd_socketpath != "" || *graphite_hostport != "" || *statsd_hostport != "" {
			ticker := time.NewTicker(time.Duration(*push_interval) * time.Second)
			go func() {
				for {
					select {
					case <-ticker.C:
						WriteMetrics()
					}
				}
			}()
		}

		log.Fatal(http.ListenAndServe(":"+*port, nil))
	}
}
