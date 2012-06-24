// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"testing"
	"time"
)

func BenchmarkIncrementScalar(b *testing.B) {
	m := &Metric{D: &Datum{}}
	ts := time.Now()
	for i := 0; i < b.N; i++ {
		m.IncBy(1, ts)
	}
}
