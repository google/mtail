// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"os"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/mtail"
	"github.com/google/mtail/testdata"
	"github.com/google/mtail/watcher"
	"github.com/kylelemons/godebug/pretty"
)

var (
	recordBenchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
)

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Sample log input.
	goldenfile  string // Expected metrics after processing.
}{
	// {
	// 	"examples/rsyncd.mtail",
	// 	"testdata/rsyncd.log",
	// 	"testdata/rsyncd.golden",
	// },
	// {
	// 	"examples/sftp.mtail",
	// 	"testdata/sftp_chroot.log",
	// 	"testdata/sftp_chroot.golden",
	// },
	{
		"examples/dhcpd.mtail",
		"testdata/anonymised_dhcpd_log",
		"testdata/anonymised_dhcpd_log.golden",
	},
	// {
	// 	"examples/ntpd.mtail",
	// 	"testdata/ntp4",
	// 	"testdata/ntp4.golden",
	// },
	// {
	// 	"examples/ntpd.mtail",
	// 	"testdata/xntp3_peerstats",
	// 	"testdata/xntp3_peerstats.golden",
	// },
	{
		"examples/otherwise.mtail",
		"testdata/otherwise.log",
		"testdata/otherwise.golden",
	},
	{
		"examples/else.mtail",
		"testdata/else.log",
		"testdata/else.golden",
	},
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}
	for _, tc := range exampleProgramTests {
		w := watcher.NewFakeWatcher()
		store := metrics.NewStore()
		o := mtail.Options{Progs: tc.programfile, W: w, Store: store}
		mtail, err := mtail.New(o)
		if err != nil {
			t.Fatalf("create mtail failed: %s", err)
		}

		if _, err := mtail.OneShot(tc.logfile, false); err != nil {
			t.Errorf("Oneshot failed for %s: %s", tc.logfile, err)
			continue
		}

		g, err := os.Open(tc.goldenfile)
		if err != nil {
			t.Fatalf("%s: could not open golden file: %s", tc.goldenfile, err)
		}
		defer g.Close()

		golden_store := metrics.NewStore()
		testdata.ReadTestData(g, tc.programfile, golden_store)

		mtail.Close()

		diff := pretty.Compare(golden_store, store)

		if len(diff) > 0 {
			t.Errorf("%s: metrics don't match:\n%s", tc.programfile, diff)

			t.Errorf("Store metrics: %#v", store.Metrics)
		}
	}
}

var benchmarks = []struct {
	programfile string
	logfile     string
}{
	{"examples/rsyncd.mtail", "testdata/rsyncd.log"},
	{"examples/sftp.mtail", "testdata/sftp_chroot.log"},
	{"examples/dhcpd.mtail", "testdata/anonymised_dhcpd_log"},
}

func BenchmarkProgram(b *testing.B) {
	for _, bm := range benchmarks {
		b.Run(bm.programfile, func(b *testing.B) {
			w := watcher.NewFakeWatcher()
			o := mtail.Options{Progs: bm.programfile, W: w}
			mtail, err := mtail.New(o)
			if err != nil {
				b.Fatalf("Failed to create mtail: %s", err)
			}

			var lines int64
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				count, err := mtail.OneShot(bm.logfile, false)
				if err != nil {
					b.Errorf("OneShot log parse failed: %s", err)
				}
				lines += count
			}
			b.StopTimer()
			mtail.Close()
			if err != nil {
				b.Fatalf("strconv.ParseInt failed: %s", err)
				return
			}
			b.SetBytes(lines)
		})
	}
}

// func BenchmarkRsyncdProgram(b *testing.B) {
// 	benchmarkProgram(b, "examples/rsyncd.mtail", "testdata/rsyncd.log")
// }
// func BenchmarkSftpProgram(b *testing.B) {
// 	benchmarkProgram(b, "examples/sftp.mtail", "testdata/sftp_chroot.log")
// }
// func BenchmarkDhcpdProgram(b *testing.B) {
// 	benchmarkProgram(b, "examples/dhcpd.mtail", "testdata/anonymised_dhcpd_log")
// }
