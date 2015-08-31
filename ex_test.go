// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"os"
	"strconv"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/mtail"
	"github.com/google/mtail/testdata"
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
		"testdata/rsyncd.golden",
	},
	// {
	// 	"examples/sftp.mtail",
	// 	"testdata/sftp_chroot.log",
	// 	"testdata/sftp_chroot.json",
	// },
	// {
	// 	"examples/dhcpd.mtail",
	// 	"testdata/anonymised_dhcpd_log",
	// 	"testdata/anonymised_dhcpd_log.json",
	// },
	// {
	// 	"examples/ntpd.mtail",
	// 	"testdata/ntp4",
	// 	"testdata/ntp4.json",
	// },
	// {
	// 	"examples/ntpd.mtail",
	// 	"testdata/xntp3_peerstats",
	// 	"testdata/xntp3_peerstats.json",
	// },
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, tc := range exampleProgramTests {
		w := watcher.NewFakeWatcher()
		store := &metrics.Store{}
		o := mtail.Options{Progs: tc.programfile, W: w, Store: store}
		mtail, err := mtail.New(o)
		if err != nil {
			t.Fatalf("create mtail failed: %s", err)
		}

		if err := mtail.OneShot(tc.logfile); err != nil {
			t.Errorf("Oneshot failed for %s: %s", tc.logfile, err)
			continue
		}

		j, err := os.Open(tc.jsonfile)
		if err != nil {
			t.Fatalf("%s: could not open json file: %s", tc.jsonfile, err)
		}
		defer j.Close()

		golden_store := &metrics.Store{}
		testdata.ReadTestData(j, tc.programfile, golden_store)

		mtail.Close()

		diff := pretty.Compare(golden_store, store)

		if len(diff) > 0 {
			t.Errorf("%s: metrics don't match:\n%s", tc.programfile, diff)

			t.Errorf("Store metrics: %#v", store.Metrics)
		}
	}
}

func benchmarkProgram(b *testing.B, programfile string, logfile string) {
	w := watcher.NewFakeWatcher()
	o := mtail.Options{Progs: programfile, W: w}
	mtail, err := mtail.New(o)
	if err != nil {
		b.Fatalf("Failed to create mtail: %s", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if err := mtail.OneShot(logfile); err != nil {
			b.Errorf("OneShot log parse failed: %s", err)
		}
	}
	b.StopTimer()
	mtail.Close()
	l, err := strconv.ParseInt(vm.LineCount.String(), 10, 64)
	if err != nil {
		b.Fatalf("strconv.ParseInt failed: %s", err)
		return
	}
	b.SetBytes(l)
}

func BenchmarkRsyncdProgram(b *testing.B) {
	benchmarkProgram(b, "examples/rsync.mtail", "testdata/rsyncd.log")
}
func BenchmarkSftpProgram(b *testing.B) {
	benchmarkProgram(b, "examples/sftp.mtail", "testdata/sftp_chroot.log")
}
func BenchmarkDhcpdProgram(b *testing.B) {
	benchmarkProgram(b, "examples/dhcpd.mtail", "testdata/anonymised_dhcpd_log")
}
