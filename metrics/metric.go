// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"errors"
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

type MetricType int

const (
	Counter MetricType = iota
	Gauge
)

var (
	Metric_update_time time.Time
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

type Incrementable interface {
	IncBy(delta int64, ts time.Time)
}

type Settable interface {
	Set(value int64, ts time.Time)
}

type LabelValue struct {
	Labels []string `json:",omitempty"`
	Value  *Datum
}

type Metric struct {
	sync.RWMutex
	Name        string // Name
	Program     string // Instantiating program
	Kind        MetricType
	Keys        []string      `json:",omitempty"`
	LabelValues []*LabelValue `json:",omitempty"`
}

func NewMetric(name string, prog string, kind MetricType, keys ...string) *Metric {
	m := &Metric{Name: name, Program: prog, Kind: kind,
		Keys:        make([]string, len(keys), len(keys)),
		LabelValues: make([]*LabelValue, 0)}
	copy(m.Keys, keys)
	return m
}

func (m *Metric) FindLabelValueOrNil(labelvalues []string) *LabelValue {
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

func (m *Metric) GetDatum(labelvalues ...string) (d *Datum, err error) {
	if len(labelvalues) != len(m.Keys) {
		return nil, errors.New(fmt.Sprintf("Label values requested (%q) not same length as keys for metric %q", labelvalues, m))
	}
	m.Lock()
	defer m.Unlock()
	if lv := m.FindLabelValueOrNil(labelvalues); lv != nil {
		d = lv.Value
	} else {
		d = &Datum{}
		m.LabelValues = append(m.LabelValues, &LabelValue{labelvalues, d})
	}
	return d, nil
}

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

func (m *Metric) EmitLabelSets(c chan *LabelSet) {
	for _, lv := range m.LabelValues {
		ls := &LabelSet{zip(m.Keys, lv.Labels), lv.Value}
		c <- ls
	}
	close(c)
}

type Datum struct {
	Value int64
	Time  time.Time
}

func (d *Datum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		d.Time = time.Now()
	} else {
		d.Time = timestamp
	}
	Metric_update_time = time.Now()
}

func (d *Datum) Set(value int64, timestamp time.Time) {
	atomic.StoreInt64(&d.Value, value)
	d.stamp(timestamp)
}

func (d *Datum) IncBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.Value, delta)
	d.stamp(timestamp)
}

func (d *Datum) Get() int64 {
	return atomic.LoadInt64(&d.Value)
}

type Store struct {
	sync.RWMutex
	Metrics []*Metric
}

func (ms *Store) Add(m ...*Metric) {
	ms.Lock()
	defer ms.Unlock()
	ms.Metrics = append(ms.Metrics, m...)
}

func (ms *Store) ClearMetrics() {
	ms.Lock()
	defer ms.Unlock()
	ms.Metrics = make([]*Metric, 0)
}

// Debug printing.
func (d *Datum) String() string {
	return fmt.Sprintf("%+#v", *d)
}

func (lv *LabelValue) String() string {
	return fmt.Sprintf("%+#v", *lv)
}

func (m *Metric) String() string {
	return fmt.Sprintf("%+#v", *m)
}

// Sort a slice of metrics.
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
