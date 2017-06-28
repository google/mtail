// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"sync"
)

// Store contains Metrics.
type Store struct {
	sync.RWMutex
	Metrics map[string][]*Metric
}

func NewStore() (s *Store) {
	s = &Store{}
	s.ClearMetrics()
	return
}

// Add is used to add one metric to the Store.
func (s *Store) Add(m *Metric) {
	s.Lock()
	defer s.Unlock()
	s.Metrics[m.Name] = append(s.Metrics[m.Name], m)
}

// ClearMetrics empties the store of all metrics.
func (s *Store) ClearMetrics() {
	s.Lock()
	defer s.Unlock()
	s.Metrics = make(map[string][]*Metric, 0)
}

func (s *Store) MarshalJSON() (b []byte, err error) {
	s.Lock()
	defer s.Unlock()
	ms := make([]*Metric, 0)
	for _, ml := range s.Metrics {
		ms = append(ms, ml...)
	}
	return json.Marshal(ms)
}
