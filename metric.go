// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"errors"
	"fmt"
	"hash/fnv"
	"sort"
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

type Metric struct {
	Name    string // Name
	Program string // Instantiating program
	Kind    metric_type
	Keys    []string
	Labels  map[uint64][]string
	Values  map[uint64]*Datum
}

func NewMetric(name string, prog string, kind metric_type, keys ...string) *Metric {
	m := &Metric{Name: name, Program: prog, Kind: kind,
		Keys:   make([]string, len(keys), len(keys)),
		Labels: make(map[uint64][]string, 0),
		Values: make(map[uint64]*Datum, 0)}
	for i, k := range keys {
		m.Keys[i] = k
	}
	return m
}

func hashLabels(values ...string) (result uint64) {
	var prime uint64 = 31
	result = 1
	for _, s := range values {
		hash := fnv.New64a()
		hash.Write([]byte(s))
		result = result*prime + hash.Sum64()
	}
	return result
}

func (m *Metric) GetDatum(labelvalues ...string) (*Datum, error) {
	if len(labelvalues) != len(m.Keys) {
		return nil, errors.New(fmt.Sprintf("Label values requested (%q) not same length as keys for metric %q", labelvalues, m))
	}
	index := hashLabels(labelvalues...)
	metric_lock.Lock()
	defer metric_lock.Unlock()
	d, ok := m.Values[index]
	if !ok {
		d = &Datum{}
		m.Labels[index] = labelvalues
		m.Values[index] = d
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

type uint64slice []uint64

func (s uint64slice) Len() int           { return len(s) }
func (s uint64slice) Less(i, j int) bool { return s[i] < s[j] }
func (s uint64slice) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

func (m *Metric) EmitLabelSets(c chan *LabelSet) {
	var keys []uint64
	for k := range m.Values {
		keys = append(keys, k)
	}
	sort.Sort(uint64slice(keys))
	for _, k := range keys {
		ls := &LabelSet{zip(m.Keys, m.Labels[k]), m.Values[k]}
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
