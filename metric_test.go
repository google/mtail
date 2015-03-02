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
	if v.Values[hashLabels()].Value != 1 {
		t.Errorf("fail")
	}
	// TODO: try setting datum with labels on scalar
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, "foo")
	d, _ := v.GetDatum("a")
	d.IncBy(1, time.Now())
	if v.Values[hashLabels("a")].Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	d.IncBy(1, time.Now())
	if v.Values[hashLabels("a", "b")].Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	d.IncBy(1, time.Now())
	if v.Values[hashLabels("a", "b", "c")].Value != 1 {
		t.Errorf("fail")
	}
}

func TestHashLabels(t *testing.T) {
	a := hashLabels()
	if a != 1 {
		t.Errorf("empty hash was %v\n", a)
	}
	b := hashLabels("a")
	c := hashLabels("a", "b")
	d := hashLabels("a", "c")
	if a == b || a == c || a == d || b == c || b == d || c == d {
		t.Errorf("hash values matched: a %v b %v c %v d %v\b", a, b, c, d)
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
	m := NewMetric("test", "prog", Gauge, "foo", "bar", "quux")
	c := make(chan *LabelSet)

	ts := time.Now()

	var expected_labels []map[string]string
	for _, tc := range labelSetTests {
		d, _ := m.GetDatum(tc.values...)
		d.Set(37, ts)
		expected_labels = append(expected_labels, tc.expected_labels)
	}

	go m.EmitLabelSets(c)

	var labels []map[string]string
	for ls := range c {
		labels = append(labels, ls.labels)
	}

	// Equivalence for slices is not defined under ==, and DeepEqual does an
	// elementwise comparison.  We can't guarantee that the labels are in
	// order, so do the N^2 comparision.
	if len(labels) != len(expected_labels) {
		t.Errorf("Label length doesn't match\n\texpected %v\n\treceived %v\n", expected_labels, labels)
	}

Loop:
	for i := range expected_labels {
		for j := range labels {
			if reflect.DeepEqual(expected_labels[i], labels[j]) {
				continue Loop
			}
		}
		t.Errorf("Labels don't match: couldn't find %v in labels\n\texpected %v\n\treceived %v\n", expected_labels[i], expected_labels, labels)

	}
}
