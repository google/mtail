// Copyright 2024 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"strconv"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
)

// TestConcurrentGetDatumAndRead exercises the store readers (Gc and the JSON
// marshalers) against concurrent GetDatum inserts.  GetDatum appends to
// Metric.LabelValues under the write lock; before the fix Gc and MarshalJSON
// read that slice without the read lock, which the race detector flags.  Run
// with -race to guard the regression.
func TestConcurrentGetDatumAndRead(t *testing.T) {
	s := NewStore()
	m := NewMetric("foo", "prog", Counter, Int, "id")
	m.Limit = 100
	testutil.FatalIfErr(t, s.Add(m))

	const iterations = 500
	var wg sync.WaitGroup

	// Writer, standing in for a VM updating the metric from log lines.
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 0; i < iterations; i++ {
			d, err := m.GetDatum(strconv.Itoa(i))
			if err == nil {
				datum.SetInt(d, 1, time.Now())
			}
		}
	}()

	// Reader 1: the periodic garbage collector.
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 0; i < iterations; i++ {
			_ = s.Gc()
		}
	}()

	// Reader 2: the /json and /debug/vars export paths.
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 0; i < iterations; i++ {
			if _, err := s.MarshalJSON(); err != nil {
				t.Errorf("MarshalJSON: %s", err)
				return
			}
		}
	}()

	wg.Wait()
}
