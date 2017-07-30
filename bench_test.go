// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Only build with go1.7 or above because b.Run did not exist before.
// +build go1.7

package main

import (
	"flag"
	"fmt"
	"testing"

	"github.com/google/mtail/mtail"
	"github.com/google/mtail/watcher"
)

var (
	recordBenchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
)

func BenchmarkProgram(b *testing.B) {
	// exampleProgramTests live in ex_test.go
	for _, bm := range exampleProgramTests {
		b.Run(fmt.Sprintf("%s on %s", bm.programfile, bm.logfile), func(b *testing.B) {
			b.ReportAllocs()

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
				b.Fatal(err)
				return
			}
			// The bytes recorded is really the number of lines read.
			b.SetBytes(lines)
		})
	}
}
