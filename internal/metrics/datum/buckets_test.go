// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum_test

import (
	"testing"
	"testing/quick"
	"time"

	"github.com/google/mtail/internal/metrics/datum"
)

func TestBucketContains(t *testing.T) {
	if err := quick.Check(func(min, max, val float64) bool {
		r := &datum.Range{Min: min, Max: max}
		truth := val < max && val >= min
		return truth == r.Contains(val)
	}, nil); err != nil {
		t.Error(err)
	}
}

func TestMakeBucket(t *testing.T) {
	r := []datum.Range{
		{0, 1},
		{1, 2},
		{2, 4},
	}
	b := datum.MakeBuckets(r, time.Unix(37, 42))
	ts := time.Unix(37, 31)
	datum.Observe(b, 1, ts)
	if r := datum.GetBucketsSum(b); r != 1 {
		t.Errorf("sum not 1, got %v", r)
	}
	if r := datum.GetBucketsCount(b); r != 1 {
		t.Errorf("count not 1, got %v", r)
	}
}
