// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import "sync"

// Store contains Metrics.
type Store struct {
	sync.RWMutex
	Metrics []*Metric
}

func NewStore() (s *Store) {
	s = &Store{}
	s.ClearMetrics()
	return
}

// Add is used to add one or more metrics in the Store.
func (ms *Store) Add(m ...*Metric) {
	ms.Lock()
	defer ms.Unlock()
	ms.Metrics = append(ms.Metrics, m...)
}

// ClearMetrics empties the store of all metrics.
func (ms *Store) ClearMetrics() {
	ms.Lock()
	defer ms.Unlock()
	ms.Metrics = make([]*Metric, 0)
}

// Metrics defines a Sortable type for a slice of metrics.
type Metrics []*Metric

func (ms Metrics) Len() int      { return len(ms) }
func (ms Metrics) Swap(i, j int) { ms[i], ms[j] = ms[j], ms[i] }
func (ms Metrics) Less(i, j int) bool {
	switch {
	case ms[i].Program < ms[j].Program:
		return true
	case ms[i].Name < ms[j].Name:
		return true
	case len(ms[i].Keys) < len(ms[j].Keys):
		return true
	default:
		return false
	}
}
