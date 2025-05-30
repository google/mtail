// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"reflect"
	"sync"
	"testing"
	"testing/quick"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestKindType(t *testing.T) {
	v := Kind(0)
	if s := v.String(); s != "Unknown" {
		t.Errorf("Kind.String() returned %q not Unknown", s)
	}
	v = Counter
	if s := v.String(); s != "Counter" {
		t.Errorf("Kind.String() returned %q not Counter", s)
	}
	v = Gauge
	if s := v.String(); s != "Gauge" {
		t.Errorf("Kind.String() returned %q not Gauge", s)
	}
	v = Timer
	if s := v.String(); s != "Timer" {
		t.Errorf("Kind.String() returned %q not Timer", s)
	}
}

func TestScalarMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, Int)
	d, err := v.GetDatum()
	if err != nil {
		t.Errorf("no datum: %s", err)
	}
	datum.IncIntBy(d, 1, time.Now().UTC())
	lv := v.FindLabelValueOrNil([]string{})
	if lv == nil {
		t.Fatal("couldn't find labelvalue")
	}
	newD := lv.Value
	if newD == nil {
		t.Error("new_d is nil")
	}
	if newD.ValueString() != "1" {
		t.Error("value not 1")
	}
	d2, err := v.GetDatum("a", "b")
	if err == nil {
		t.Errorf("datum with keys sohuld have returned no value, got %v", d2)
	}
}

func TestDimensionedMetric(t *testing.T) {
	v := NewMetric("test", "prog", Counter, Int, "foo")
	d, _ := v.GetDatum("a")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.FindLabelValueOrNil([]string{"a"}).Value.ValueString() != "1" {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, Int, "foo", "bar")
	d, _ = v.GetDatum("a", "b")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.FindLabelValueOrNil([]string{"a", "b"}).Value.ValueString() != "1" {
		t.Errorf("fail")
	}

	v = NewMetric("test", "prog", Counter, Int, "foo", "bar", "quux")
	d, _ = v.GetDatum("a", "b", "c")
	datum.IncIntBy(d, 1, time.Now().UTC())
	if v.FindLabelValueOrNil([]string{"a", "b", "c"}).Value.ValueString() != "1" {
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
	ts := time.Now().UTC()
	for _, tc := range labelSetTests {
		tc := tc
		t.Run(fmt.Sprintf("%v", tc.values), func(t *testing.T) {
			m := NewMetric("test", "prog", Gauge, Int, "foo", "bar", "quux")
			d, _ := m.GetDatum(tc.values...)
			datum.SetInt(d, 37, ts)

			c := make(chan *LabelSet)

			go m.EmitLabelSets(c)

			ls := <-c

			testutil.ExpectNoDiff(t, tc.expectedLabels, ls.Labels)
		})
	}
}

func TestFindLabelValueOrNil(t *testing.T) {
	m0 := NewMetric("foo", "prog", Counter, Int)
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
	m1 := NewMetric("bar", "prog", Counter, Int, "a")
	d1, err1 := m1.GetDatum("1")
	if err1 != nil {
		t.Errorf("err1 %v: %v\n", d1, err1)
	}
	if r2 := m1.FindLabelValueOrNil([]string{"0"}); r2 != nil {
		t.Errorf("r2 should be nil")
	}
	if r3 := m1.FindLabelValueOrNil([]string{"1"}); r3 == nil {
		t.Errorf("r3 should be non nil")
	}
}

func TestAppendLabelValue(t *testing.T) {
	m := NewMetric("foo", "prog", Counter, Int, "bar")
	l := []string{"test"}
	d0 := datum.MakeInt(66, time.Unix(0, 0))
	lv := &LabelValue{Labels: l, Value: d0}
	err := m.AppendLabelValue(lv)
	if err != nil {
		t.Errorf("Bad append %v: %v\n", d0, err)
	}
	d1, err := m.GetDatum(l...)
	if err != nil {
		t.Errorf("Bad datum %v: %v\n", d1, err)
	}
	testutil.ExpectNoDiff(t, d0, d1)
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
	f := func(name, prog string, kind Kind, keys []string, val, _, _ int64) bool {
		m := NewMetric(name, prog, kind, Int, keys...)
		labels := make([]string, 0)
		for range keys {
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

		r := newMetric(0)
		e = json.Unmarshal(j, &r)
		if e != nil {
			t.Errorf("json.Unmarshal failed: %s\n", e)
			return false
		}

		return testutil.ExpectNoDiff(t, m, r, testutil.IgnoreUnexported(sync.RWMutex{}, Metric{}))
	}
	if err := quick.Check(f, nil); err != nil {
		t.Error(err)
	}
}

func TestTimer(t *testing.T) {
	m := NewMetric("test", "prog", Timer, Int)
	n := NewMetric("test", "prog", Timer, Int)
	testutil.ExpectNoDiff(t, m, n, testutil.IgnoreUnexported(sync.RWMutex{}, Metric{}))
	d, _ := m.GetDatum()
	datum.IncIntBy(d, 1, time.Now().UTC())
	lv := m.FindLabelValueOrNil([]string{})
	if lv == nil {
		t.Fatal("couldn't find labelvalue")
	}
	newD := lv.Value
	if newD == nil {
		t.Errorf("new_d is nil")
	}
	if newD.ValueString() != "1" {
		t.Errorf("value not 1")
	}
}

func TestRemoveMetricLabelValue(t *testing.T) {
	m := NewMetric("test", "prog", Counter, Int, "a", "b", "c")
	_, e := m.GetDatum("a", "a", "a")
	if e != nil {
		t.Errorf("Getdatum failed: %s", e)
	}
	lv := m.FindLabelValueOrNil([]string{"a", "a", "a"})
	if lv == nil {
		t.Errorf("coidln't find labelvalue")
	}
	e = m.RemoveDatum("a", "a", "a")
	if e != nil {
		t.Errorf("couldn't remove datum: %s", e)
	}
	lv = m.FindLabelValueOrNil([]string{"a", "a", "a"})
	if lv != nil {
		t.Errorf("label value still exists")
	}
}

func TestMetricLabelValueRemovePastLimit(t *testing.T) {
	m := NewMetric("test", "prog", Counter, Int, "foo")
	m.Limit = 1
	_, err := m.GetDatum("a")
	testutil.FatalIfErr(t, err)
	m.RemoveOldestDatum()
	_, err = m.GetDatum("b")
	testutil.FatalIfErr(t, err)
	m.RemoveOldestDatum()
	_, err = m.GetDatum("c")
	testutil.FatalIfErr(t, err)
	m.RemoveOldestDatum()
	if len(m.LabelValues) > 2 {
		t.Errorf("Expected 2 labelvalues got %#v", m.LabelValues)
	}
	if x := m.FindLabelValueOrNil([]string{"a"}); x != nil {
		t.Errorf("found label a which is unexpected: %#v", x)
	}
}
