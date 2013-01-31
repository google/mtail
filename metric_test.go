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
	v := NewMetric("test", "prog", Counter)
	d, _ := v.GetDatum()
	d.IncBy(1, time.Now())
	if v.Values.D.Value != 1 {
		t.Errorf("fail")
	}
	// TODO: try setting datum with labels on scalar
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, "foo")
	d, _ := v.GetDatum("a")
	d.IncBy(1, time.Now())
	if v.Values.Next["a"].D.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	d.IncBy(1, time.Now())
	if v.Values.Next["a"].Next["b"].D.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	d.IncBy(1, time.Now())
	if v.Values.Next["a"].Next["b"].Next["c"].D.Value != 1 {
		t.Errorf("fail")
	}
}

var labelSetTests = []struct {
	values          []string
	expected_labels map[string]string
}{
	{
		[]string{"a", "b", "c"},
		map[string]string{"foo": "a", "bar": "b", "quux": "c"},
	},
	{
		[]string{"a", "b", "d"},
		map[string]string{"foo": "a", "bar": "b", "quux": "d"},
	},
}

func TestEmitLabelSet(t *testing.T) {
	v := NewMetric("test", "prog", Gauge, "foo", "bar", "quux")
	c := make(chan *LabelSet)

	quit := make(chan bool)
	ts := time.Now()

	for _, tc := range labelSetTests {
		d, _ := v.GetDatum(tc.values...)
		d.Set(37, ts)
	}
	go v.EmitLabelSets(c, quit)
	expected_datum := &Datum{37, ts}
	for _, tc := range labelSetTests {
		select {
		case l := <-c:
			if !reflect.DeepEqual(expected_datum, l.datum) {
				t.Errorf("Datum no match: expected %v, received %v\n", expected_datum, l.datum)
			}
			if !reflect.DeepEqual(tc.expected_labels, l.labels) {
				t.Errorf("Labels don't match: expected %v, received %v\n", tc.expected_labels, l.labels)
			}
		case <-quit:
			goto out
		}
	}
out:
}
