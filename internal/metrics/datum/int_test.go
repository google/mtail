// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"testing"
	"time"
)

func BenchmarkIncrementScalarInt(b *testing.B) {
	d := &Int{}
	ts := time.Now().UTC()
	for i := 0; i < b.N; i++ {
		d.IncBy(1, ts)
	}
}

func BenchmarkDecrementScalarInt(b *testing.B) {
	d := &Int{}
	ts := time.Now().UTC()
	for i := 0; i < b.N; i++ {
		d.DecBy(1, ts)
	}
}

func TestDecrementScalarInt(t *testing.T) {
	d := &Int{}
	ts := time.Now().UTC()
	d.IncBy(1, ts)
	r := d.Get()
	if r != 1 {
		t.Errorf("expected 1, got %d", r)
	}
	d.DecBy(1, ts)
	r = d.Get()
	if r != 0 {
		t.Errorf("expected 0, got %d", r)
	}
}
