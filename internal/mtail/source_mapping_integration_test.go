// Copyright 2023 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestSourceMappingIntegration(t *testing.T) {
	// Create test programs
	progCounterA := `
counter counter_a
/test/ {
  counter_a++
}
`
	progCounterB := `
counter counter_b
/test/ {
  counter_b++
}
`
	
	// Set up directories
	workdir := testutil.TestTempDir(t)
	
	// Create program files
	progdir := filepath.Join(workdir, "progs")
	err := os.Mkdir(progdir, 0o755)
	if err != nil {
		t.Fatal(err)
	}
	
	err = os.WriteFile(filepath.Join(progdir, "counter_a.mtail"), []byte(progCounterA), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	err = os.WriteFile(filepath.Join(progdir, "counter_b.mtail"), []byte(progCounterB), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Create log files
	logdir := filepath.Join(workdir, "logs")
	err = os.Mkdir(logdir, 0o755)
	if err != nil {
		t.Fatal(err)
	}
	
	logfileA := filepath.Join(logdir, "log_a.log")
	err = os.WriteFile(logfileA, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	logfileB := filepath.Join(logdir, "log_b.log")
	err = os.WriteFile(logfileB, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	logfileUnmapped := filepath.Join(logdir, "log_unmapped.log")
	err = os.WriteFile(logfileUnmapped, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Create source mapping file
	mappingFile := filepath.Join(workdir, "source_mapping.yaml")
	mappingContent := `
unmapped_behavior: "all"
mappings:
  - source: "` + logfileA + `"
    programs:
      - "counter_a.mtail"
  - source: "` + logfileB + `"
    programs:
      - "counter_b.mtail"
`
	err = os.WriteFile(mappingFile, []byte(mappingContent), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Start mtail - using 1 pattern waker for the log directory glob pattern
	ts, stopFunc := mtail.TestStartServer(t, 1, 0, 
		mtail.ProgramPath(progdir), 
		mtail.LogPathPatterns(logdir+"/*"),
		mtail.SourceMappingFile(mappingFile),
	)
	defer stopFunc()
	
	// Write to log files and check metrics
	// Log A should trigger counter_a but not counter_b
	err = os.WriteFile(logfileA, []byte("test line for log A\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Log B should trigger counter_b but not counter_a
	err = os.WriteFile(logfileB, []byte("test line for log B\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Log unmapped should trigger both counters (default behavior is "all")
	err = os.WriteFile(logfileUnmapped, []byte("test line for unmapped log\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Wait for mtail to process the logs
	time.Sleep(1 * time.Second)
	
	// Get the metrics from the store field
	store := ts.store
	
	// Check if counter_a and counter_b were incremented
	counterA := false
	counterB := false
	
	store.Range(func(m *metrics.Metric) error {
		if m.Name == "counter_a" {
			// We expect 2 increments: 1 from log A + 1 from unmapped log
			v := datum.GetInt(m.LabelValues[0].Value)
			if v == 2 {
				counterA = true
			}
		}
		if m.Name == "counter_b" {
			// We expect 2 increments: 1 from log B + 1 from unmapped log
			v := datum.GetInt(m.LabelValues[0].Value)
			if v == 2 {
				counterB = true
			}
		}
		return nil
	})
	
	if !counterA || !counterB {
		t.Error("Did not receive expected metrics")
	}
	
	// Now test with unmapped_behavior set to "none"
	// Create new mapping file
	mappingFileNone := filepath.Join(workdir, "source_mapping_none.yaml")
	mappingContentNone := `
unmapped_behavior: "none"
mappings:
  - source: "` + logfileA + `"
    programs:
      - "counter_a.mtail"
  - source: "` + logfileB + `"
    programs:
      - "counter_b.mtail"
`
	err = os.WriteFile(mappingFileNone, []byte(mappingContentNone), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Restart mtail with new mapping
	stopFunc()
	ts, stopFunc = mtail.TestStartServer(t, 1, 0, 
		mtail.ProgramPath(progdir), 
		mtail.LogPathPatterns(logdir+"/*"),
		mtail.SourceMappingFile(mappingFileNone),
	)
	defer stopFunc()
	
	// Reset log files
	err = os.WriteFile(logfileA, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(logfileB, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(logfileUnmapped, []byte(""), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Write to log files again
	err = os.WriteFile(logfileA, []byte("test line for log A\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(logfileB, []byte("test line for log B\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(logfileUnmapped, []byte("test line for unmapped log\n"), 0o644)
	if err != nil {
		t.Fatal(err)
	}
	
	// Wait for mtail to process the logs
	time.Sleep(1 * time.Second)
	
	// Get the metrics from the store field
	store = ts.store
	
	// Check if counter_a and counter_b were incremented correctly
	counterA = false
	counterB = false
	
	store.Range(func(m *metrics.Metric) error {
		if m.Name == "counter_a" {
			// We expect 1 increment, only from log A (unmapped log ignored)
			v := datum.GetInt(m.LabelValues[0].Value)
			if v == 1 {
				counterA = true
			}
		}
		if m.Name == "counter_b" {
			// We expect 1 increment, only from log B (unmapped log ignored)
			v := datum.GetInt(m.LabelValues[0].Value)
			if v == 1 {
				counterB = true
			}
		}
		return nil
	})
	
	if !counterA || !counterB {
		t.Error("Did not receive expected metrics with unmapped_behavior=none")
	}
}