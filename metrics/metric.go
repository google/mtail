// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package metrics provides storage for metrics being recorded by mtail
// programs.
package metrics

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

// MetricType enumerates the types of metrics supported.
type MetricType int

const (
	// Counter is a MetricType that is nondecreasing, typically only
	// incrementable.
	Counter MetricType = iota
	// Gauge is a MetricType that can take on any value, and may be set
	// discontinuously from its previous value.
	Gauge
)

var (
	// MetricUpdateTime contains the timestamp of the last update of a metric.
	// TODO(jaq): move this global value to be a property of the Store.
	MetricUpdateTime atomic.Value
)

func (m MetricType) String() string {
	switch m {
	case Counter:
		return "Counter"
	case Gauge:
		return "Gauge"
	}
	return "Unknown"
}

// Incrementable describes an interface for Counter MetricTypes, that must be
// nondecreasing.
type Incrementable interface {
	IncBy(delta int64, ts time.Time)
}

// Settable describes an interface for Gauge MetricTypes, that can be set to
// any value discontinuously from its previous.
type Settable interface {
	Set(value int64, ts time.Time)
}

// LabelValue is an object that names a Datum value with a list of label
// strings.
type LabelValue struct {
	Labels []string `json:",omitempty"`
	Value  *Datum
}

// Metric is an object that describes a metric, with its name, the creator and
// owner program name, its MetricType, a sequence of Keys that may be used to
// add dimension to the metric, and a list of LabelValues that contain data for
// labels in each dimension of the Keys.
type Metric struct {
	sync.RWMutex
	Name        string // Name
	Program     string // Instantiating program
	Kind        MetricType
	Keys        []string      `json:",omitempty"`
	LabelValues []*LabelValue `json:",omitempty"`
}

// NewMetric returns a new empty metric of dimension len(keys).
func NewMetric(name string, prog string, kind MetricType, keys ...string) *Metric {
	m := &Metric{Name: name, Program: prog, Kind: kind,
		Keys:        make([]string, len(keys), len(keys)),
		LabelValues: make([]*LabelValue, 0)}
	copy(m.Keys, keys)
	return m
}

func (m *Metric) findLabelValueOrNil(labelvalues []string) *LabelValue {
Loop:
	for i, lv := range m.LabelValues {
		for j := 0; j < len(lv.Labels); j++ {
			if lv.Labels[j] != labelvalues[j] {
				continue Loop
			}
		}
		return m.LabelValues[i]
	}
	return nil
}

// GetDatum returns the datum named by a sequence of string label values from a
// Metric.
func (m *Metric) GetDatum(labelvalues ...string) (d *Datum, err error) {
	if len(labelvalues) != len(m.Keys) {
		return nil, fmt.Errorf("Label values requested (%q) not same length as keys for metric %q", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
	if lv := m.findLabelValueOrNil(labelvalues); lv != nil {
		d = lv.Value
	} else {
		d = &Datum{}
		m.LabelValues = append(m.LabelValues, &LabelValue{labelvalues, d})
	}
	return d, nil
}

// LabelSet is an object that maps the keys of a Metric to the labels naming a
// Datum, for use when enumerating Datums from a Metric.
type LabelSet struct {
	Labels map[string]string
	Datum  *Datum
}

func zip(keys []string, values []string) map[string]string {
	r := make(map[string]string, 0)
	for i, v := range values {
		r[keys[i]] = v
	}
	return r
}

// EmitLabelSets enumerates the LabelSets corresponding to the LabelValues of a
// Metric.  It emits them onto the provided channel, then closes the channel to
// signal completion.
func (m *Metric) EmitLabelSets(c chan *LabelSet) {
	for _, lv := range m.LabelValues {
		ls := &LabelSet{zip(m.Keys, lv.Labels), lv.Value}
		c <- ls
	}
	close(c)
}

// Datum describes a LabelSet's or LabelValue's value at a given timestamp.
type Datum struct {
	sync.RWMutex
	Value int64
	Time  time.Time
}

func (d *Datum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		d.Time = time.Now().UTC()
	} else {
		d.Time = timestamp
	}
	MetricUpdateTime.Store(time.Now().UTC())
}

// Set implements the Settable interface for a Datum.
func (d *Datum) Set(value int64, timestamp time.Time) {
	d.Lock()
	defer d.Unlock()
	d.Value = value
	d.stamp(timestamp)
}

// IncBy implements the Incrementable interface for a Datum.
func (d *Datum) IncBy(delta int64, timestamp time.Time) {
	d.Lock()
	defer d.Unlock()
	d.Value += delta
	d.stamp(timestamp)
}

// Get returns the value of the Datum.
func (d *Datum) Get() int64 {
	d.RLock()
	defer d.RUnlock()
	return d.Value
}

// Store contains Metrics.
type Store struct {
	sync.RWMutex
	Metrics []*Metric
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

func (d *Datum) String() string {
	d.RLock()
	defer d.RUnlock()
	return fmt.Sprintf("%+#v", *d)
}

func (lv *LabelValue) String() string {
	return fmt.Sprintf("%+#v", *lv)
}

func (m *Metric) String() string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%+#v", *m)
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
