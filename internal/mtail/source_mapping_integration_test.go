// Copyright 2023 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"os"
	"path/filepath"
	"testing"
	"time"

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
	
	// Start mtail
	m, err := mtail.TestStartServer(t, 
		mtail.ProgramPath(progdir), 
		mtail.LogPathPatterns(logdir+"/*"),
		mtail.SourceMappingFile(mappingFile),
	)
	if err != nil {
		t.Fatal(err)
	}
	defer m.Close()
	
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
	
	// Check metrics
	counterA := m.ExpectMetricDeltaWithDeadline("counter_a", 2.0) // 1 from log A + 1 from unmapped
	counterB := m.ExpectMetricDeltaWithDeadline("counter_b", 2.0) // 1 from log B + 1 from unmapped
	
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
	m.Close()
	m, err = mtail.TestStartServer(t, 
		mtail.ProgramPath(progdir), 
		mtail.LogPathPatterns(logdir+"/*"),
		mtail.SourceMappingFile(mappingFileNone),
	)
	if err != nil {
		t.Fatal(err)
	}
	defer m.Close()
	
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
	
	// Check metrics - unmapped log should not trigger any counters now
	counterA = m.ExpectMetricDeltaWithDeadline("counter_a", 1.0) // Only from log A
	counterB = m.ExpectMetricDeltaWithDeadline("counter_b", 1.0) // Only from log B
	
	if !counterA || !counterB {
		t.Error("Did not receive expected metrics with unmapped_behavior=none")
	}
}