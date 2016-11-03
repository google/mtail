// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Only build with go1.7 or above because b.Run did not exist before.
// +build go1.7

package main

import (
	"flag"
	"path/filepath"
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
		prog := filepath.Base(bm.programfile)
		log := filepath.Base(bm.logfile)
		b.Run(prog+"+"+log, func(b *testing.B) {
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
