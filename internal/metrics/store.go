// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"context"
	"encoding/json"
	"io"
	"reflect"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/pkg/errors"
)

// Store contains Metrics.
type Store struct {
	searchMu sync.RWMutex // read for iterate and insert, write for delete
	insertMu sync.Mutex   // locked for insert and delete, unlocked for iterate
	Metrics  map[string][]*Metric
}

// NewStore returns a new metric Store.
func NewStore() (s *Store) {
	s = &Store{}
	s.ClearMetrics()
	return
}

// Add is used to add one metric to the Store.
func (s *Store) Add(m *Metric) error {
	s.insertMu.Lock()
	defer s.insertMu.Unlock()
	s.searchMu.RLock()
	glog.V(1).Infof("Adding a new metric %v", m)
	dupeIndex := -1
	if len(s.Metrics[m.Name]) > 0 {
		t := s.Metrics[m.Name][0].Kind
		if m.Kind != t {
			s.searchMu.RUnlock()
			return errors.Errorf("metric %s has different kind %v to existing %v", m.Name, m.Kind, t)
		}

		// To avoid duplicate metrics:
		// - copy old LabelValues into new metric;
		// - discard old metric.
		for i, v := range s.Metrics[m.Name] {
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
			glog.V(2).Infof("v buckets: %v m.buckets: %v", v.Buckets, m.Buckets)

			// Otherwise, copy everything into the new metric
			glog.V(2).Infof("Found duped metric: %d", dupeIndex)
			for j, oldLabel := range v.LabelValues {
				glog.V(2).Infof("Labels: %d %s", j, oldLabel.Labels)
				d, err := v.GetDatum(oldLabel.Labels...)
				if err != nil {
					return err
				}
				if err = m.RemoveDatum(oldLabel.Labels...); err != nil {
					return err
				}
				lv := &LabelValue{Labels: oldLabel.Labels, Value: d}
				if err := m.AppendLabelValue(lv); err != nil {
					return err
				}
			}
		}
	}
	s.searchMu.RUnlock()

	// We're in modify mode now so lock out search
	s.searchMu.Lock()
	s.Metrics[m.Name] = append(s.Metrics[m.Name], m)
	if dupeIndex >= 0 {
		glog.V(2).Infof("removing original, keeping its clone")
		s.Metrics[m.Name] = append(s.Metrics[m.Name][0:dupeIndex], s.Metrics[m.Name][dupeIndex+1:]...)
	}
	s.searchMu.Unlock()
	return nil
}

// FindMetricOrNil returns a metric in a store, or returns nil if not found.
func (s *Store) FindMetricOrNil(name, prog string) *Metric {
	s.searchMu.RLock()
	defer s.searchMu.RUnlock()
	ml, ok := s.Metrics[name]
	if !ok {
		return nil
	}
	for _, m := range ml {
		if m.Program != prog {
			continue
		}
		return m
	}
	return nil
}

// ClearMetrics empties the store of all metrics.
func (s *Store) ClearMetrics() {
	s.insertMu.Lock()
	defer s.insertMu.Unlock()
	s.searchMu.Lock()
	defer s.searchMu.Unlock()
	s.Metrics = make(map[string][]*Metric)
}

// MarshalJSON returns a JSON byte string representing the Store.
func (s *Store) MarshalJSON() (b []byte, err error) {
	s.searchMu.RLock()
	defer s.searchMu.RUnlock()
	ms := make([]*Metric, 0)
	for _, ml := range s.Metrics {
		ms = append(ms, ml...)
	}
	return json.Marshal(ms)
}

// Range calls f sequentially for each Metric present in the store.
// The Metric is not locked when f is called.
// If f returns non nil error, Range stops the iteration.
// This looks a lot like sync.Map, ay.
func (s *Store) Range(f func(*Metric) error) error {
	s.searchMu.RLock()
	defer s.searchMu.RUnlock()
	for _, ml := range s.Metrics {
		for _, m := range ml {
			if err := f(m); err != nil {
				return err
			}
		}
	}
	return nil
}

// Gc iterates through the Store looking for metrics that can be tidied up,
// if they are passed their expiry or sized greater than their limit.
func (s *Store) Gc() error {
	glog.Info("Running Store.Expire()")
	now := time.Now()
	return s.Range(func(m *Metric) error {
		if m.Limit > 0 && len(m.LabelValues) >= m.Limit {
			for i := len(m.LabelValues); i > m.Limit; i-- {
				m.RemoveOldestDatum()
			}
		}
		for i := 0; i < len(m.LabelValues); i++ {
			lv := m.LabelValues[i]
			if lv.Expiry <= 0 {
				continue
			}
			if now.Sub(lv.Value.TimeUTC()) > lv.Expiry {
				err := m.RemoveDatum(lv.Labels...)
				if err != nil {
					return err
				}
				i--
			}
		}
		return nil
	})
}

// StartGcLoop runs a permanent goroutine to expire metrics every duration.
func (s *Store) StartGcLoop(ctx context.Context, duration time.Duration) {
	if duration <= 0 {
		glog.Infof("Metric store expiration disabled")
		return
	}
	go func() {
		glog.Infof("Starting metric store expiry loop every %s", duration.String())
		ticker := time.NewTicker(duration)
		defer ticker.Stop()
		for {
			select {
			case <-ticker.C:
				if err := s.Gc(); err != nil {
					glog.Info(err)
				}
			case <-ctx.Done():
				return
			}
		}
	}()
}

// WriteMetrics dumps the current state of the metrics store in JSON format to
// the io.Writer.
func (s *Store) WriteMetrics(w io.Writer) error {
	s.searchMu.RLock()
	b, err := json.MarshalIndent(s.Metrics, "", "  ")
	s.searchMu.RUnlock()
	if err != nil {
		return errors.Wrap(err, "failed to marshal metrics into json")
	}
	_, err = w.Write(b)
	if err != nil {
		return errors.Wrap(err, "failed to write metrics")
	}
	return nil
}
