// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"sync"
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

type Node struct {
	D    *Datum
	Next map[string]*Node
}

type Metric struct {
	Name   string
	Kind   metric_type
	Keys   []string
	Values *Node
}

func NewMetric(name string, kind metric_type, keys ...string) *Metric {
	m := &Metric{Name: name, Kind: kind,
		Keys:   make([]string, len(keys), len(keys)),
		Values: &Node{}}
	for i, k := range keys {
		m.Keys[i] = k
	}
	if len(keys) > 0 {
		m.Values.Next = make(map[string]*Node, 0)
	}
	return m
}

func (m *Metric) GetDatum(labelvalues ...string) *Datum {
	if len(labelvalues) > len(m.Keys) {
		return nil
	}
	n := m.Values
	for _, l := range labelvalues {
		if tmp, ok := n.Next[l]; !ok {
			metric_lock.Lock()
			n.Next[l] = &Node{Next: make(map[string]*Node, 0)}
			metric_lock.Unlock()
			n = n.Next[l]
		} else {
			n = tmp
		}
	}
	if n.D == nil {
		metric_lock.Lock()
		n.D = &Datum{}
		metric_lock.Unlock()
	}
	return n.D
}

func (m *Metric) IterLabels() map[string]string {
	return nil
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
	metric_lock.Lock()
	defer metric_lock.Unlock()
	d.Value = value
	d.stamp(timestamp)
}

func (d *Datum) IncBy(delta int64, timestamp time.Time) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	d.Value += delta
	d.stamp(timestamp)
}

func init() {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	metrics = make([]*Metric, 0)
}

func ExportMetric(m *Metric) (addr int) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	addr = len(metrics)
	metrics = append(metrics, m)
	return
}
