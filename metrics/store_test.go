// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import "testing"

func TestMatchingKind(t *testing.T) {
	s := NewStore()
	m1 := NewMetric("foo", "prog", Counter, Int)
	err := s.Add(m1)
	if err != nil {
		t.Fatalf("should be nil: %s", err)
	}
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
		t.Fatalf("should not add duplicate metric. Store: %s", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Float))
	t.Logf("Store: %s", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %s", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Int, "user", "host", "zone", "domain"))
	t.Logf("Store: %s", s)
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should not add duplicate metric, but replace the old one. Store: %s", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Int))
	t.Logf("Store: %s", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric with a different prog: %s", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Float))
	t.Logf("Store: %s", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %s", s)
	}
}
