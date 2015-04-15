// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"testing"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/vm"
	"github.com/google/mtail/watcher"
	"github.com/kylelemons/godebug/pretty"
)

var (
	recordBenchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
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

func CompileAndLoad(programfile string, ms *metrics.Store, lines chan string) (*vm.Loader, error) {
	p, err := os.Open(programfile)
	if err != nil {
		return nil, fmt.Errorf("%s: could not open program file: %s", programfile, err)
	}
	defer p.Close()

	name := filepath.Base(programfile)
	w := watcher.NewFakeWatcher()
	l := vm.NewLoader(w, ms, lines)
	if l == nil {
		return nil, fmt.Errorf("couldn't create program loader")
	}
	if pErr := l.CompileAndRun(name, p); pErr != nil {
		return nil, fmt.Errorf("couldn't compile program: %s: %s", programfile, pErr)
	}
	return l, nil
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	*vm.SyslogUseCurrentYear = false
	for _, tc := range exampleProgramTests {
		mtail := newMtail()
		l, err := CompileAndLoad(tc.programfile, &mtail.store, mtail.lines)
		if err != nil {
			t.Fatalf("Compile failed: %s", err)
		}
		mtail.l = l

		if err := mtail.OneShot(tc.logfile, mtail.lines); err != nil {
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
			b, err := json.MarshalIndent(mtail.store.Metrics, "", "  ")
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

		var expectedMetrics []*metrics.Metric

		d := json.NewDecoder(j)
		err = d.Decode(&expectedMetrics)
		if err != nil {
			t.Errorf("%s: could not decode json: %s", tc.jsonfile, err)
			continue
		}
		mtail.Close()
		sort.Sort(metrics.Metrics(expectedMetrics))
		mtail.store.Lock()
		sort.Sort(metrics.Metrics(mtail.store.Metrics))
		diff := pretty.Compare(mtail.store.Metrics, expectedMetrics)
		mtail.store.Unlock()
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
		mtail := newMtail()
		spareLines := make(chan string)
		l, err := CompileAndLoad(tc.programfile, &metrics.Store{}, spareLines)
		if err != nil {
			b.Errorf("Compile failed: %s", err)
		}
		mtail.l = l
		r := testing.Benchmark(func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				b.StopTimer()
				vm.LineCount.Set(0)
				mtail.store.ClearMetrics()
				b.StartTimer()
				err := mtail.OneShot(tc.logfile, spareLines)
				if err != nil {
					b.Errorf("OneShot log parse failed: %s", err)
					return
				}
				b.StopTimer()
				l, err := strconv.ParseInt(vm.LineCount.String(), 10, 64)
				if err != nil {
					b.Errorf("strconv.ParseInt failed: %s", err)
					return
				}
				b.SetBytes(l)
			}
		})
		close(spareLines)

		klPerSecond := float64(r.Bytes) * float64(r.N) / (r.T.Seconds() * 1000)
		msPerRun := float64(r.NsPerOp()) / 1e6
		lr := r.Bytes * int64(r.N)
		µsPerL := float64(r.T.Nanoseconds()) / (float64(r.Bytes) * float64(r.N) * 1000)
		fmt.Printf("%s: %d runs, %d lines in %s (%f ms/run, %d lines/run, %f Klines/s, %f µs/line)\n",
			tc.programfile, r.N, lr, r.T, msPerRun, r.Bytes, klPerSecond, µsPerL)
		if *recordBenchmark {
			f, err := os.OpenFile("benchmark_results.csv", os.O_RDWR|os.O_CREATE|os.O_APPEND, 0600)
			if err != nil {
				fmt.Printf("benchmark write failed: %s\n", err)
				continue
			}
			defer f.Close()
			c := csv.NewWriter(f)
			defer c.Flush()
			record := func(v ...interface{}) []string {
				var r []string
				for _, x := range v {
					r = append(r, fmt.Sprintf("%v", x))
				}
				return r
			}(time.Now().Unix(),
				runtime.GOMAXPROCS(-1),
				runtime.NumCPU(),
				tc.programfile,
				r.N, lr, r.T, msPerRun, r.Bytes, klPerSecond, µsPerL)
			// Format is time, concurrency, number of cores,
			// name, data
			err = c.Write(record)
			if err != nil {
				fmt.Printf("failed to write csv record %q: %s\n", record, err)
			}
		}
	}
}
