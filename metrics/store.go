// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"encoding/json"
	"reflect"
	"sync"

	"github.com/golang/glog"
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
	glog.Infof("Adding a new metric %v", m)
	dupeIndex := -1
	if len(s.Metrics[m.Name]) > 0 {
		t := s.Metrics[m.Name][0].Kind
		if m.Kind != t {
			return errors.Errorf("Metric %s has different kind %s to existing %s.", m.Name, m.Kind, t)
		}

		// To avoid duplicate metrics:
		// - copy old LabelValues into new metric;
		// - discard old metric.
		for i, v := range s.Metrics[m.Name] {
			//
			if v.Program != m.Program {
				continue
			}
			if v.Type != m.Type {
				continue
			}
			if v.Source != m.Source {
				continue
			}
			dupeIndex = i
			glog.V(2).Infof("v keys: %v m.keys: %v", v.Keys, m.Keys)
			// If a set of label keys has changed, discard
			// old metric completely, w/o even copying old
			// data, as they are now incompatible.
			if len(v.Keys) != len(m.Keys) || !reflect.DeepEqual(v.Keys, m.Keys) {
				break
			}
			// Otherwise, copy everything into the new metric
			glog.V(2).Infof("Found duped metric: %d", dupeIndex)
			for j, oldLabel := range v.LabelValues {
				glog.V(2).Infof("Labels: %d %s", j, oldLabel.Labels)
				d, err := v.GetDatum(oldLabel.Labels...)
				if err == nil {
					if err = m.RemoveDatum(oldLabel.Labels...); err == nil {
						m.LabelValues = append(m.LabelValues, &LabelValue{oldLabel.Labels, d})
					}
				}
			}
		}
	}

	s.Metrics[m.Name] = append(s.Metrics[m.Name], m)
	if dupeIndex >= 0 {
		s.Metrics[m.Name] = append(s.Metrics[m.Name][0:dupeIndex], s.Metrics[m.Name][dupeIndex+1:]...)
	}
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
