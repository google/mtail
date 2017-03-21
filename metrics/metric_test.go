// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"math/rand"
	"reflect"
	"testing"
	"testing/quick"
	"time"

	"github.com/google/mtail/metrics/datum"
	"github.com/kylelemons/godebug/pretty"
)

func TestScalarMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, Int)
	d, _ := v.GetDatum()
	datum.IncIntBy(d, 1, time.Now().UTC())
	lv := v.findLabelValueOrNil([]string{})
	if lv == nil {
		t.Errorf("couldn't find labelvalue")
	}
	newD := lv.Value
	if newD == nil {
		t.Errorf("new_d is nil")
	}
	if newD.Value() != "1" {
		t.Errorf("value not 1")
	}
	// TODO: try setting datum with labels on scalar
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, Int, "foo")
	d, _ := v.GetDatum("a")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.findLabelValueOrNil([]string{"a"}).Value.Value() != "1" {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, Int, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.findLabelValueOrNil([]string{"a", "b"}).Value.Value() != "1" {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, Int, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.findLabelValueOrNil([]string{"a", "b", "c"}).Value.Value() != "1" {
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
	m := NewMetric("test", "prog", Gauge, Int, "foo", "bar", "quux")
	c := make(chan *LabelSet)

	ts := time.Now().UTC()

	var expectedLabels []map[string]string
	for _, tc := range labelSetTests {
		d, _ := m.GetDatum(tc.values...)
		datum.SetInt(d, 37, ts)
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
	m0 := NewMetric("foo", "prog", Counter, Int)
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
	m1 := NewMetric("bar", "prog", Counter, Int, "a")
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

func timeGenerator(rand *rand.Rand) time.Time {
	months := []time.Month{
		time.January, time.February, time.March,
		time.April, time.May, time.June,
		time.July, time.August, time.September,
		time.October, time.November, time.December,
	}

	return time.Date(
		rand.Intn(9999),
		months[rand.Intn(len(months))],
		rand.Intn(31),
		rand.Intn(24),
		rand.Intn(60),
		rand.Intn(60),
		int(rand.Int31()),
		time.UTC,
	)
}

func TestMetricJSONRoundTrip(t *testing.T) {
	rand := rand.New(rand.NewSource(0))
	f := func(name, prog string, kind Kind, keys []string, val, ti, tns int64) bool {
		m := NewMetric(name, prog, kind, Int, keys...)
		var labels []string
		for _ = range keys {
			if l, ok := quick.Value(reflect.TypeOf(name), rand); ok {
				labels = append(labels, l.String())
			} else {
				t.Errorf("failed to create value for labels")
				break
			}
		}
		d, _ := m.GetDatum(labels...)
		datum.SetInt(d, val, timeGenerator(rand))

		j, e := json.Marshal(m)
		if e != nil {
			t.Errorf("json.Marshal failed: %s\n", e)
			return false
		}

		r := &Metric{}
		e = json.Unmarshal(j, &r)
		if e != nil {
			t.Errorf("json.Unmarshal failed: %s\n", e)
			return false
		}

		// pretty.Compare uses the opposite order to xUnit for comparisons.
		diff := pretty.Compare(m, r)
		if len(diff) > 0 {
			t.Errorf("Round trip wasn't stable:\n%s", diff)
			return false
		}
		return true
	}
	q := quick.Config{MaxCount: 100000}
	if testing.Short() {
		q.MaxCount = 1000
	}
	if err := quick.Check(f, nil); err != nil {
		t.Error(err)
	}
}

func TestTimer(t *testing.T) {
	m := NewMetric("test", "prog", Timer, Int)
	n := NewMetric("test", "prog", Timer, Int)
	diff := pretty.Compare(m, n)
	if len(diff) > 0 {
		t.Errorf("Identical metrics not the same:\n%s", diff)
	}
	d, _ := m.GetDatum()
	datum.IncIntBy(d, 1, time.Now().UTC())
	lv := m.findLabelValueOrNil([]string{})
	if lv == nil {
		t.Errorf("couldn't find labelvalue")
	}
	newD := lv.Value
	if newD == nil {
		t.Errorf("new_d is nil")
	}
	if newD.Value() != "1" {
		t.Errorf("value not 1")
	}
}

func TestRemoveMetricLabelValue(t *testing.T) {
	m := NewMetric("test", "prog", Counter, Int, "a", "b", "c")
	_, e := m.GetDatum("a", "a", "a")
	if e != nil {
		t.Errorf("Getdatum failed: %s", e)
	}
	lv := m.findLabelValueOrNil([]string{"a", "a", "a"})
	if lv == nil {
		t.Errorf("coidln't find labelvalue")
	}
	e = m.RemoveDatum("a", "a", "a")
	if e != nil {
		t.Errorf("couldn't remove datum: %s", e)
	}
	lv = m.findLabelValueOrNil([]string{"a", "a", "a"})
	if lv != nil {
		t.Errorf("label value still exists")
	}
}
