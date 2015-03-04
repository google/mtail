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
	lv := v.FindLabelValueOrNil([]string{})
	if lv == nil {
		t.Errorf("couldn't find labelvalue")
	}
	new_d := lv.Value
	if new_d == nil {
		t.Errorf("new_d is nil")
	}
	if new_d.Value != 1 {
		t.Errorf("value not 1")
	}
	// TODO: try setting datum with labels on scalar
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, "foo")
	d, _ := v.GetDatum("a")
	d.IncBy(1, time.Now())
	if v.FindLabelValueOrNil([]string{"a"}).Value.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	d.IncBy(1, time.Now())
	if v.FindLabelValueOrNil([]string{"a", "b"}).Value.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	d.IncBy(1, time.Now())
	if v.FindLabelValueOrNil([]string{"a", "b", "c"}).Value.Value != 1 {
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

func TestFindLabelValueOrNil(t *testing.T) {
	m0 := NewMetric("foo", "prog", Counter)
	if r0 := m0.FindLabelValueOrNil([]string{}); r0 != nil {
		t.Errorf("m0 should be nil: %v", r0)
	}
	d, err := m0.GetDatum()
	if err != nil {
		t.Errorf("Bad datum %v: %v\n", d, err)
	}
	if r1 := m0.FindLabelValueOrNil([]string{}); r1 == nil {
		t.Errorf("m0 should not be nil: %v", r1)
	}
}
