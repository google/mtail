// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Only build with go1.7 or above because b.Run did not exist before.
// +build integration

package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path"
	"testing"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/watcher"
	"github.com/spf13/afero"
)

var (
	recordBenchmark = flag.Bool("record_benchmark", false, "Record the benchmark results to 'benchmark_results.csv'.")
)

func BenchmarkProgram(b *testing.B) {
	// exampleProgramTests live in ex_test.go
	for _, bm := range exampleProgramTests {
		bm := bm
		b.Run(fmt.Sprintf("%s on %s", bm.programfile, bm.logfile), func(b *testing.B) {
			b.ReportAllocs()
			logDir, rmLogDir := mtail.TestTempDir(b)
			defer rmLogDir()
			logFile := path.Join(logDir, "test.log")
			log := mtail.TestOpenFile(b, logFile)
			w := watcher.NewFakeWatcher()
			store := metrics.NewStore()
			mtail, err := mtail.New(store, w, afero.OsFs{}, mtail.ProgramPath(bm.programfile), mtail.LogPathPatterns(log.Name()))
			if err != nil {
				b.Fatalf("Failed to create mtail: %s", err)
			}
			err = mtail.StartTailing()
			if err != nil {
				b.Fatalf("starttailing failed: %s", err)
			}

			var total int64
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				l, err := os.Open(bm.logfile)
				if err != nil {
					b.Fatalf("Couldn't open logfile: %s", err)
				}
				count, err := io.Copy(log, l)
				if err != nil {
					b.Fatalf("Write of test data failed to test file: %s", err)
				}
				total += count
				w.InjectUpdate(log.Name())
			}
			mtail.Close()
			b.StopTimer()
			b.SetBytes(total)
		})
	}
}
