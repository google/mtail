// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"sync"

	"github.com/pkg/errors"
)

// Store contains Metrics.
type Store struct {
	sync.RWMutex
	Metrics map[string][]*Metric
}

// NewStore returns a new metric Store.
func NewStore() (s *Store) {
	s = &Store{}
	s.ClearMetrics()
	return
}

// Add is used to add one metric to the Store.
func (s *Store) Add(m *Metric) error {
	s.Lock()
	defer s.Unlock()
	if len(s.Metrics[m.Name]) > 0 {
		t := s.Metrics[m.Name][0].Kind
		if m.Kind != t {
			return errors.Errorf("Metric %s has different kind %s to existing %s.", m.Name, m.Kind, t)
		}
	}
	s.Metrics[m.Name] = append(s.Metrics[m.Name], m)
	return nil
}

// ClearMetrics empties the store of all metrics.
func (s *Store) ClearMetrics() {
	s.Lock()
	defer s.Unlock()
	s.Metrics = make(map[string][]*Metric)
}

// MarshalJSON returns a JSON byte string representing the Store.
func (s *Store) MarshalJSON() (b []byte, err error) {
	s.Lock()
	defer s.Unlock()
	ms := make([]*Metric, 0)
	for _, ml := range s.Metrics {
		ms = append(ms, ml...)
	}
	return json.Marshal(ms)
}
