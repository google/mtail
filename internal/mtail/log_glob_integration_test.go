// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail_test

import (
	"path"
	"sync"
	"testing"

	"github.com/google/mtail/internal/mtail"
	"github.com/google/mtail/internal/testutil"
)

func TestGlobBeforeStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			true,
		},
		{
			path.Join(workdir, "log2"),
			true,
		},
		{
			path.Join(workdir, "1log"),
			false,
		},
	}
	count := 0
	for _, tt := range globTests {
		log := testutil.TestOpenFile(t, tt.name)
		defer log.Close()
		if tt.expected {
			count++
		}
		testutil.WriteString(t, log, "\n")
	}
	m, stopM := mtail.TestStartServer(t, 0, true, mtail.LogPathPatterns(path.Join(workdir, "log*")))
	defer stopM()

	if r := m.GetMetric("log_count"); r != float64(count) {
		t.Errorf("Expecting log count of %d, received %g", count, r)
	}
}

func TestGlobAfterStart(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	workdir, rmWorkdir := testutil.TestTempDir(t)
	defer rmWorkdir()

	globTests := []struct {
		name     string
		expected bool
	}{
		{
			path.Join(workdir, "log1"),
			true,
		},
		{
			path.Join(workdir, "log2"),
			true,
		},
		{
			path.Join(workdir, "1log"),
			false,
		},
	}
	m, stopM := mtail.TestStartServer(t, 0, false, mtail.LogPathPatterns(path.Join(workdir, "log*")))
	defer stopM()

	count := 0
	for _, tt := range globTests {
		if tt.expected {
			count++
		}
	}
	logCountCheck := m.ExpectMetricDeltaWithDeadline("log_count", float64(count))
	linesCountCheck := m.ExpectMetricDeltaWithDeadline("lines_total", float64(count))
	for _, tt := range globTests {
		log := testutil.TestOpenFile(t, tt.name)
		defer log.Close()
		testutil.WriteString(t, log, "\n")
	}
	m.PollWatched()
	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		linesCountCheck()
	}()
	go func() {
		defer wg.Done()
		logCountCheck()
	}()
	wg.Wait()
}
