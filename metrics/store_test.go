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
