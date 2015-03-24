// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/vm"
	"github.com/kylelemons/godebug/pretty"
)

var (
	record_benchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
)

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Sample log input.
	jsonfile    string // Expected metrics after processing.
}{
	{
		"examples/rsyncd.mtail",
		"testdata/rsyncd.log",
		"testdata/rsyncd.json",
	},
	{
		"examples/sftp.mtail",
		"testdata/sftp_chroot.log",
		"testdata/sftp_chroot.json",
	},
	{
		"examples/dhcpd.mtail",
		"testdata/anonymised_dhcpd_log",
		"testdata/anonymised_dhcpd_log.json",
	},
}

func CompileAndLoad(programfile string, stop chan bool) (chan string, string) {
	lines := make(chan string)

	p, err := os.Open(programfile)
	if err != nil {
		return lines, fmt.Sprintf("%s: could not open program file: %s", programfile, err)
	}
	defer p.Close()

	// MtailDebug = 999 // All the debugging.

	v, errs := vm.Compile(programfile, p)
	if errs != nil {
		return lines, fmt.Sprintf("%s: compile failed: %s", programfile, strings.Join(errs, "\n"))
	}

	e := &vm.Engine{}
	e.AddVm(programfile, v)
	go e.Run(lines, stop)
	return lines, ""
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	*vm.Syslog_use_current_year = false
	for _, tc := range exampleProgramTests {
		stop := make(chan bool, 1)
		lines, errs := CompileAndLoad(tc.programfile, stop)
		if errs != "" {
			t.Errorf(errs)
			continue
		}

		m := &mtail{}

		err := m.OneShot(tc.logfile, lines, stop)
		if err != nil {
			t.Errorf("Oneshot failed for %s: %s", tc.logfile, err)
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
			b, err := json.MarshalIndent(m.store.Metrics, "", "  ")
			if err != nil {
				t.Errorf("couldn't marshall metrics: %q", err)
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

		var expected_metrics []*metrics.Metric

		d := json.NewDecoder(j)
		err = d.Decode(&expected_metrics)
		if err != nil {
			t.Errorf("%s: could not decode json: %s", tc.jsonfile, err)
			continue
		}
		sort.Sort(metrics.Metrics(expected_metrics))
		sort.Sort(metrics.Metrics(m.store.Metrics))
		diff := pretty.Compare(expected_metrics, m.store.Metrics)
		if len(diff) > 0 {
			t.Errorf("%s: metrics don't match:\n%s\n", tc.programfile, diff)
		}
	}
}

// These benchmarks run the testdata logs through the example programs.
func BenchmarkExamplePrograms(b *testing.B) {
	if testing.Short() {
		b.Skip("skipping test in short mode")
	}
	b.Logf("\n")
	for _, tc := range exampleProgramTests {
		stop := make(chan bool, 1)
		lines, errs := CompileAndLoad(tc.programfile, stop)
		if errs != "" {
			b.Errorf(errs)
			continue
		}
		r := testing.Benchmark(func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				b.StopTimer()
				stop_fake := make(chan bool, 1)
				vm.Line_count.Set(0)
				m := &mtail{}
				b.StartTimer()
				err := m.OneShot(tc.logfile, lines, stop_fake)
				if err != nil {
					b.Errorf("OneShot log parse failed: %s", err)
					return
				}
				b.StopTimer()
				l, err := strconv.ParseInt(vm.Line_count.String(), 10, 64)
				if err != nil {
					b.Errorf("strconv.ParseInt failed: %s", err)
					return
				}
				b.SetBytes(l)
			}
		})
		stop <- true

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
				fmt.Printf("failed to write csv record %q: %s\n", record, err)
			}
		}
	}
}
