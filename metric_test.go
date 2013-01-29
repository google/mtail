// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"testing"
	"time"
)

func BenchmarkIncrementScalar(b *testing.B) {
	d := &Datum{}
	ts := time.Now()
	for i := 0; i < b.N; i++ {
		d.IncBy(1, ts)
	}
}

func TestScalarMetric(t *testing.T) {
	v := NewMetric("test", Counter)
	v.GetDatum().IncBy(1, time.Now())
	if v.Values.D.Value != 1 {
		t.Errorf("fail")
	}
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", Counter, "foo")
	v.GetDatum("a").IncBy(1, time.Now())
	if v.Values.Next["a"].D.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", Counter, "foo", "bar")
	v.GetDatum("a", "b").IncBy(1, time.Now())
	if v.Values.Next["a"].Next["b"].D.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", Counter, "foo", "bar", "quux")
	v.GetDatum("a", "b", "c").IncBy(1, time.Now())
	if v.Values.Next["a"].Next["b"].Next["c"].D.Value != 1 {
		t.Errorf("fail")
	}
}
