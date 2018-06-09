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
		t.Fatalf("should not add duplicate metric. Store: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Float))
	t.Logf("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog", Counter, Int, "user", "host", "zone", "domain"))
	t.Logf("Store: %v", s)
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should not add duplicate metric, but replace the old one. Store: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Int))
	t.Logf("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric with a different prog: %v", s)
	}

	_ = s.Add(NewMetric("foo", "prog1", Counter, Float))
	t.Logf("Store: %v", s)
	expectedMetrics++
	if len(s.Metrics["foo"]) != expectedMetrics {
		t.Fatalf("should add metric of a different type: %v", s)
	}
}

/* A program can add a metric with the same name and
   of different type.
   Prometheus behavior in this case is undefined.
   @see https://github.com/google/mtail/issues/130
*/
func TestAddMetricDifferentType(t *testing.T) {
	expected := 2
	s := NewStore()
	err := s.Add(NewMetric("foo", "prog", Counter, Int))
	if err != nil {
		t.Fatalf("should be nil: %s", err)
	}
	// Duplicate metric of different type from *the same program
	err = s.Add(NewMetric("foo", "prog", Counter, Float))
	if err != nil {
		t.Fatalf("should add a new metric to the store: %s. Store: %s", err, s.Metrics)
	}
	if len(s.Metrics["foo"]) != expected {
		t.Fatalf("should have %d metrics of different Type: %s", expected, s.Metrics)
	}

	// Duplicate metric of different type from a different program
	err = s.Add(NewMetric("foo", "prog1", Counter, Float))
	expected++
	if err != nil {
		t.Fatalf("should add a new metric to the store: %s. Store: %s", err, s.Metrics)
	}
	if len(s.Metrics["foo"]) != expected {
		t.Fatalf("should have %d metrics of different Type: %s", expected, s.Metrics)
	}
}
