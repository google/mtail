// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum_test

import (
	"math"
	"testing"
	"testing/quick"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestRangeContains(t *testing.T) {
	if err := quick.Check(func(lo, hi, val float64) bool {
		r := &datum.Range{Min: lo, Max: hi}
		truth := val < hi && val >= lo
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
	datum.Observe(b, 2, ts)
	if r := datum.GetBucketsSum(b); r != 2 {
		t.Errorf("sum not 2, got %v", r)
	}
	if r := datum.GetBucketsCount(b); r != 1 {
		t.Errorf("count not 1, got %v", r)
	}
	bs := datum.GetBucketsCumByMax(b)
	expected := map[float64]uint64{
		1:            0,
		2:            1,
		4:            1,
		math.Inf(+1): 1,
	}
	testutil.ExpectNoDiff(t, expected, bs)
}
