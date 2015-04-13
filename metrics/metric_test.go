// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/kylelemons/godebug/pretty"
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
	lv := v.findLabelValueOrNil([]string{})
	if lv == nil {
		t.Errorf("couldn't find labelvalue")
	}
	newD := lv.Value
	if newD == nil {
		t.Errorf("new_d is nil")
	}
	if newD.Value != 1 {
		t.Errorf("value not 1")
	}
	// TODO: try setting datum with labels on scalar
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, "foo")
	d, _ := v.GetDatum("a")
	d.IncBy(1, time.Now())
	if v.findLabelValueOrNil([]string{"a"}).Value.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	d.IncBy(1, time.Now())
	if v.findLabelValueOrNil([]string{"a", "b"}).Value.Value != 1 {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	d.IncBy(1, time.Now())
	if v.findLabelValueOrNil([]string{"a", "b", "c"}).Value.Value != 1 {
		t.Errorf("fail")
	}
}

var labelSetTests = []struct {
	values         []string
	expectedLabels map[string]string
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

	var expectedLabels []map[string]string
	for _, tc := range labelSetTests {
		d, _ := m.GetDatum(tc.values...)
		d.Set(37, ts)
		expectedLabels = append(expectedLabels, tc.expectedLabels)
	}

	go m.EmitLabelSets(c)

	var labels []map[string]string
	for ls := range c {
		labels = append(labels, ls.Labels)
	}

	diff := pretty.Compare(labels, expectedLabels)
	if len(diff) > 0 {
		t.Errorf("Labels don't match:\n%s", diff)
	}
}

func TestFindLabelValueOrNil(t *testing.T) {
	m0 := NewMetric("foo", "prog", Counter)
	if r0 := m0.findLabelValueOrNil([]string{}); r0 != nil {
		t.Errorf("m0 should be nil: %v", r0)
	}
	d, err := m0.GetDatum()
	if err != nil {
		t.Errorf("Bad datum %v: %v\n", d, err)
	}
	if r1 := m0.findLabelValueOrNil([]string{}); r1 == nil {
		t.Errorf("m0 should not be nil: %v", r1)
	}
	m1 := NewMetric("bar", "prog", Counter, "a")
	d1, err1 := m1.GetDatum("1")
	if err1 != nil {
		t.Errorf("err1 %v: %v\n", d1, err1)
	}
	if r2 := m1.findLabelValueOrNil([]string{"0"}); r2 != nil {
		t.Errorf("r2 should be nil")
	}
	if r3 := m1.findLabelValueOrNil([]string{"1"}); r3 == nil {
		t.Errorf("r3 should be non nil")
	}
}

func TestMetricJSONRoundTrip(t *testing.T) {
	m := NewMetric("test", "prog", Gauge, "foo", "bar", "quux")
	d, _ := m.GetDatum("a", "2", "d")
	d.Set(1, time.Now().UTC())

	j, e := json.Marshal(m)
	if e != nil {
		t.Errorf("json.Marshal failed: %s\n", e)
	}

	r := &Metric{}
	e = json.Unmarshal(j, &r)
	if e != nil {
		t.Errorf("json.Unmarshal failed: %s\n", e)
	}

	// pretty.Compare uses the opposite order to xUnit for comparisons.
	diff := pretty.Compare(r, m)
	if len(diff) > 0 {
		t.Errorf("Round trip wasn't stable:\n%s", diff)
	}
}
