// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"testing"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
)

func TestMatchingKind(t *testing.T) {
	s := NewStore()
	m1 := NewMetric("foo", "prog", Counter, Int)
	err := s.Add(m1)
	testutil.FatalIfErr(t, err)
	m2 := NewMetric("foo", "prog", Gauge, Int)
	err = s.Add(m2)
	if err == nil {
		t.Fatal("should be err")
	}
}

func TestDuplicateMetric(t *testing.T) {
	s := NewStore()

	m1 := NewMetric("foo", "prog", Counter, Int, "user", "host")
	if err := s.Add(m1); err != nil {
		t.Error(err)
	}

	if err := s.Add(NewMetric("foo", "prog", Counter, Float)); err != nil {
		t.Error(err)
	}
	glog.Infof("Store: %v", s)

	if err := s.Add(NewMetric("foo", "prog", Counter, Int, "user", "host", "zone", "domain")); err != nil {
		t.Error(err)
	}
	m2 := s.FindMetricOrNil("foo", "prog")
	if m2 == nil {
		t.Errorf("couldn't find metric in %v", s)
	}
	if len(m2.Keys) != 4 {
		t.Fatalf("should not add duplicate metric, but replace the old one: %v Store: %v", m2, s)
	}

	if err := s.Add(NewMetric("foo", "prog1", Counter, Int)); err != nil {
		t.Errorf("should add metric with a different prog: %s", err)
	}
}

func TestExpireMetric(t *testing.T) {
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
		t.Errorf("couldn't find lv")
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
