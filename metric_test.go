// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
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

func TestEmitLabelSet(t *testing.T) {
	v := NewMetric("test", Gauge, "foo", "bar", "quux")
	ts := time.Now()
	v.GetDatum("a", "b", "c").Set(37, ts)
	c := make(chan LabelSet, 0)
	quit := make(chan bool)
	go v.EmitLabelSets(c, quit)
	expected_datum := &Datum{37, ts}
	expected_labels := map[string]string{"foo": "a", "bar": "b", "quux": "c"}
	select {
	case l := <-c:
		if !reflect.DeepEqual(expected_datum, l.datum) {
			t.Errorf("Datum no match: expected %v, received %v\n", expected_datum, l.datum)
		}
		if !reflect.DeepEqual(expected_labels, l.labels) {
			t.Errorf("Labels don't match: expected %v, received %v\n", expected_labels, l.labels)
		}
	case <-quit:
		goto out
	}
out:
}
