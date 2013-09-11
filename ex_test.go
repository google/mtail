// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"
)

var (
	record_benchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
)

// Debug printing.
func (d *Datum) String() string {
	return fmt.Sprintf("%v", *d)
}

func (n *Node) String() string {
	return fmt.Sprintf("%v", *n)
}

func (m *Metric) String() string {
	return fmt.Sprintf("%v", *m)
}

// Sort a slice of metrics.
type Metrics []*Metric

func (ms Metrics) Len() int           { return len(ms) }
func (ms Metrics) Swap(i, j int)      { ms[i], ms[j] = ms[j], ms[i] }
func (ms Metrics) Less(i, j int) bool { return ms[i].Name < ms[j].Name }

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Sample log input.
	jsonfile    string // Expected metrics after processing.
}{
	{
		"examples/linecount.em",
		"testdata/linecount.log",
		"testdata/linecount.json",
	},
	{
		"examples/rsyncd.em",
		"testdata/rsyncd.log",
		"testdata/rsyncd.json",
	},
	{
		"examples/sftp.em",
		"testdata/sftp_chroot.log",
		"testdata/sftp_chroot.json",
	},
	{
		"examples/dhcpd.em",
		"testdata/anonymised_dhcpd_log",
		"testdata/anonymised_dhcpd_log.json",
	},
}

func CompileAndLoad(programfile string) (chan string, string) {
	lines := make(chan string)

	p, err := os.Open(programfile)
	if err != nil {
		return lines, fmt.Sprintf("%s: could not open program file: %s", programfile, err)
	}
	defer p.Close()

	// EmtailDebug = 999 // All the debugging.

	v, errs := Compile(programfile, p)
	if errs != nil {
		return lines, fmt.Sprintf("%s: compile failed: %s", programfile, strings.Join(errs, "\n"))
	}

	e := &engine{}
	e.addVm(programfile, v)
	go e.run(lines)
	return lines, ""
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		return
	}
	*syslog_use_current_year = false
	for _, tc := range exampleProgramTests {
		metrics = make([]*Metric, 0)
		lines, errs := CompileAndLoad(tc.programfile)
		if errs != "" {
			t.Errorf(errs)
			continue
		}

		err := OneShot(tc.logfile, lines)
		if err != nil {
			t.Errorf("Oneshot failed: %s", err)
			continue
		}

		// Dirty hack to create json files :)
		if false {
			j, err := os.Create(tc.jsonfile)
			if err != nil {
				t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
				continue
			}
			defer j.Close()
			b, err := json.MarshalIndent(metrics, "", "  ")
			if err != nil {
				t.Errorf("couldn't marshall metrics")
				continue
			}
			j.Write(b)
		}

		j, err := os.Open(tc.jsonfile)
		if err != nil {
			t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
			continue
		}
		defer j.Close()

		var expected_metrics []*Metric

		d := json.NewDecoder(j)
		err = d.Decode(&expected_metrics)
		if err != nil {
			t.Errorf("%s: could not decode json: %s", tc.jsonfile, err)
			continue
		}
		sort.Sort(Metrics(expected_metrics))
		sort.Sort(Metrics(metrics))
		metric_lock.Lock()
		defer metric_lock.Unlock()
		if !reflect.DeepEqual(expected_metrics, metrics) {
			t.Errorf("%s: metrics don't match.\n\texpected:\n%q\n\treceived:\n%q", tc.programfile, expected_metrics, metrics)
		}
	}
}

// These benchmarks run the testdata logs through the example programs.
func BenchmarkExamplePrograms(b *testing.B) {
	if testing.Short() {
		return
	}
	b.Logf("\n")
	for _, tc := range exampleProgramTests {
		lines, errs := CompileAndLoad(tc.programfile)
		if errs != "" {
			b.Errorf(errs)
		}
		r := testing.Benchmark(func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				b.StopTimer()
				line_count.Set(0)
				b.StartTimer()
				err := OneShot(tc.logfile, lines)
				if err != nil {
					b.Errorf("OneShot compile failed: %s", err)
					return
				}
				b.StopTimer()
				l, err := strconv.ParseInt(line_count.String(), 10, 64)
				if err != nil {
					b.Errorf("strconv.ParseInt failed: %s", err)
				}
				b.SetBytes(l)
			}
		})

		kl_s := float64(r.Bytes) * float64(r.N) / (r.T.Seconds() * 1000)
		ms_run := float64(r.NsPerOp()) / 1e6
		lr := r.Bytes * int64(r.N)
		µs_l := float64(r.T.Nanoseconds()) / (float64(r.Bytes) * float64(r.N) * 1000)
		fmt.Printf("%s: %d runs, %d lines in %s (%f ms/run, %d lines/run, %f Klines/s, %f µs/line)\n",
			tc.programfile, r.N, lr, r.T, ms_run, r.Bytes, kl_s, µs_l)
		if *record_benchmark {
			f, err := os.OpenFile("benchmark_results.csv", os.O_RDWR|os.O_CREATE|os.O_APPEND, 0600)
			if err != nil {
				fmt.Printf("benchmark write failed: %s\n", err)
				continue
			}
			defer f.Close()
			c := csv.NewWriter(f)
			defer c.Flush()
			record := func(v ...interface{}) []string {
				r := make([]string, 0)
				for _, x := range v {
					r = append(r, fmt.Sprintf("%v", x))
				}
				return r
			}(time.Now().Unix(),
				runtime.GOMAXPROCS(-1),
				runtime.NumCPU(),
				tc.programfile,
				r.N, lr, r.T, ms_run, r.Bytes, kl_s, µs_l)
			// Format is time, concurrency, number of cores,
			// name, data
			err = c.Write(record)
			if err != nil {
				fmt.Printf("failed to write csv record %q: %s\n", err)
			}
		}
	}
}
