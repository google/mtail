// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"strconv"
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestMatchingKind(t *testing.T) {
	s := NewStore()
	m1 := NewMetric("foo", "prog", Counter, Int)
	err := s.Add(m1)
	testutil.FatalIfErr(t, err)
	m2 := NewMetric("foo", "prog1", Gauge, Int)
	err = s.Add(m2)
	if err == nil {
		t.Fatal("should be err")
	}
}

func TestDuplicateMetric(t *testing.T) {
	expectedMetrics := 0
	s := NewStore()
	_ = s.Add(NewMetric("foo", "prog", Counter, Int, "user", "host"))
	_ = s.Add(NewMetric("foo", "prog", Counter, Int))
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should not add duplicate metric. Store: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Float))
	glog.Infof("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Int, "user", "host", "zone", "domain"))
	glog.Infof("Store: %v", s)
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should not add duplicate metric, but replace the old one. Store: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Int))
	glog.Infof("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric with a different prog: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Float))
	glog.Infof("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %v", s)
	}
}

// A program can add a metric with the same name and of different type.
// Prometheus behavior in this case is undefined.  @see
// https://github.com/jaqx0r/mtail/issues/130
func TestAddMetricDifferentType(t *testing.T) {
	expected := 2
	s := NewStore()
	err := s.Add(NewMetric("foo", "prog", Counter, Int))
	testutil.FatalIfErr(t, err)
	// Duplicate metric of different type from *the same program
	err = s.Add(NewMetric("foo", "prog", Counter, Float))
	testutil.FatalIfErr(t, err)
	if len(s.Metrics["foo"]) != expected {
		t.Fatalf("should have %d metrics of different Type: %v", expected, s.Metrics)
	}

	// Duplicate metric of different type from a different program
	err = s.Add(NewMetric("foo", "prog1", Counter, Float))
	expected++
	testutil.FatalIfErr(t, err)
	if len(s.Metrics["foo"]) != expected {
		t.Fatalf("should have %d metrics of different Type: %v", expected, s.Metrics)
	}
}

func TestExpireOldDatum(t *testing.T) {
	s := NewStore()
	m := NewMetric("foo", "prog", Counter, Int, "a", "b", "c")
	testutil.FatalIfErr(t, s.Add(m))
	d, err := m.GetDatum("1", "2", "3")
	if err != nil {
		t.Error(err)
	}
	datum.SetInt(d, 1, time.Now().Add(-time.Hour))
	lv := m.FindLabelValueOrNil([]string{"1", "2", "3"})
	if lv == nil {
		t.Fatal("couldn't find lv")
	}
	lv.Expiry = time.Minute
	d, err = m.GetDatum("4", "5", "6")
	if err != nil {
		t.Error(err)
	}
	datum.SetInt(d, 1, time.Now().Add(-time.Hour))
	lv = m.FindLabelValueOrNil([]string{"4", "5", "6"})
	if lv == nil {
		t.Errorf("couldn't find lv")
	}

	testutil.FatalIfErr(t, s.Gc())
	lv = m.FindLabelValueOrNil([]string{"1", "2", "3"})
	if lv != nil {
		t.Errorf("lv not expired: %#v", lv)
		t.Logf("Store: %#v", s)
	}
	lv = m.FindLabelValueOrNil([]string{"4", "5", "6"})
	if lv == nil {
		t.Errorf("lv expired")
		t.Logf("Store: %#v", s)
	}
}

func TestExpireOversizeDatum(t *testing.T) {
	s := NewStore()
	m := NewMetric("foo", "prog", Counter, Int, "foo")
	m.Limit = 1
	testutil.FatalIfErr(t, s.Add(m))

	_, err := m.GetDatum("a")
	testutil.FatalIfErr(t, err)
	testutil.FatalIfErr(t, s.Gc())

	_, err = m.GetDatum("b")
	testutil.FatalIfErr(t, err)
	testutil.FatalIfErr(t, s.Gc())

	_, err = m.GetDatum("c")
	testutil.FatalIfErr(t, err)
	testutil.FatalIfErr(t, s.Gc())

	if len(m.LabelValues) > 2 {
		t.Errorf("Expected 2 labelvalues got %#v", m.LabelValues)
	}
	if x := m.FindLabelValueOrNil([]string{"a"}); x != nil {
		t.Errorf("found label a which is unexpected: %#v", x)
	}
}

func TestExpireManyMetrics(t *testing.T) {
	s := NewStore()
	m := NewMetric("foo", "prog", Counter, Int, "id")
	testutil.FatalIfErr(t, s.Add(m))
	d, err := m.GetDatum("0")
	if err != nil {
		t.Error(err)
	}
	datum.SetInt(d, 1, time.Now().Add(-time.Hour))
	lv := m.FindLabelValueOrNil([]string{"0"})
	if lv == nil {
		t.Fatal("couldn't find lv")
	}

	for i := 1; i < 10; i++ {
		d, err := m.GetDatum(strconv.Itoa(i))
		if err != nil {
			t.Error(err)
		}
		datum.SetInt(d, 1, time.Now().Add(-time.Hour))
		lv = m.FindLabelValueOrNil([]string{strconv.Itoa(i)})
		if lv == nil {
			t.Fatal("couldn't find lv")
		}
		lv.Expiry = time.Minute
	}

	testutil.FatalIfErr(t, s.Gc())
	lv = m.FindLabelValueOrNil([]string{"8"})
	if lv != nil {
		t.Errorf("lv not expired: %#v", lv)
		t.Logf("Store: %#v", s)
	}
	lv = m.FindLabelValueOrNil([]string{"0"})
	if lv == nil {
		t.Errorf("lv expired")
		t.Logf("Store: %#v", s)
	}
}
