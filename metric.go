// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"errors"
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

type metric_type int

const (
	Counter metric_type = iota
	Gauge
)

var (
	metric_lock        sync.RWMutex
	metrics            []*Metric
	metric_update_time time.Time
)

func (m metric_type) String() string {
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
	Name        string // Name
	Program     string // Instantiating program
	Kind        metric_type
	Keys        []string      `json:",omitempty"`
	LabelValues []*LabelValue `json:",omitempty"`
}

func NewMetric(name string, prog string, kind metric_type, keys ...string) *Metric {
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
	metric_lock.Lock()
	defer metric_lock.Unlock()
	if lv := m.FindLabelValueOrNil(labelvalues); lv != nil {
		d = lv.Value
	} else {
		d = &Datum{}
		m.LabelValues = append(m.LabelValues, &LabelValue{labelvalues, d})
	}
	return d, nil
}

type LabelSet struct {
	labels map[string]string
	datum  *Datum
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
	metric_update_time = time.Now()
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

func ClearMetrics() {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	metrics = make([]*Metric, 0)
}

func init() {
	ClearMetrics()
}

func ExportMetric(m *Metric) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	metrics = append(metrics, m)
}
