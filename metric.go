// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"strings"
	"sync"
	"time"
)

type metric_type int

const (
	Counter metric_type = iota
	Gauge
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
	Inc(ts time.Time)
	IncBy(delta int64, ts time.Time)
}

type Settable interface {
	Set(value int64, ts time.Time)
}

type Metric interface{}

type MetricBase struct {
	Name string
	Kind metric_type
}

type ScalarMetric struct {
	MetricBase
	d Datum
}

func (m *ScalarMetric) Inc(ts time.Time) {
	m.d.Inc(ts)
}

func (m *ScalarMetric) stamp(ts time.Time) {
	m.d.stamp(ts)
}

func (m *ScalarMetric) IncBy(delta int64, ts time.Time) {
	m.d.IncBy(delta, ts)
}

func (m *ScalarMetric) Set(value int64, ts time.Time) {
	m.d.Set(value, ts)
}

type DimensionedMetric struct {
	MetricBase
	Keys   []string
	Values map[string]*Datum
}

type Datum struct {
	Value int64
	Time  time.Time
}

// Global metrics storage.
var (
	metric_lock sync.RWMutex
	metrics     map[string]*Metric
)

const KEY_HASH_SEP = " "

func key_hash(keys []string) string {
	return strings.Join(keys, KEY_HASH_SEP)
}

func key_unhash(key string) []string {
	return strings.Split(key, KEY_HASH_SEP)
}

// func Fetch(name string, keys []string) (datum *Datum) {
// 	key := key_hash(keys)
// 	m, ok := metrics[name]
// 	if !ok {
// 		m.Values[key] = &Datum{}
// 		datum := n.Values[key]
// 	} else {
// 		return n.Values[key]
// 	}
// }

// func Store(name string, keys []string, datum *Datum) {
// 	if n, ok := metrics[name]; ok {
// 		key := key_hash(keys)
// 		n.Values[key] = datum
// 	}
// }

func (d *Datum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		d.Time = time.Now()
	} else {
		d.Time = timestamp
	}
}

func (d *Datum) Set(value int64, timestamp time.Time) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	d.Value = value
	d.stamp(timestamp)
}

func (d *Datum) Inc(timestamp time.Time) {
	d.IncBy(1, timestamp)
}

func (d *Datum) IncBy(delta int64, timestamp time.Time) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	d.Value += delta
	d.stamp(timestamp)
}

// func (m *Metric) Inc(delta int64, timestamp time.Time) {
// 	if len(m.Keys) > 0 {
// 		panic("can't increment a dimensional metric")
// 	}
// 	key := " " // Dirty hack.
// 	d, ok := m.Values[key]
// 	if !ok {
// 		d = &Datum{}
// 		m.Values[key] = d
// 	}
// 	d.Inc(delta, timestamp)
// }

// func (m *Metric) Set(value int64, timestamp time.Time) {
// 	if len(m.Keys) > 0 {
// 		panic("can't set a dimensional metric")
// 	}
// 	key := " " // Dirty Hack
// 	d, ok := m.Values[key]
// 	if !ok {
// 		d = &Datum{}
// 		m.Values[key] = d
// 	}
// 	d.Set(value, timestamp)
// }

// func (m *Metric) IncD(keys []string, delta int64, timestamp time.Time) {
// 	if len(m.Keys) == 0 {
// 		panic("cannot set with keys a nondimensional metric")
// 	}
// 	key := key_hash(keys)
// 	d, ok := m.Values[key]
// 	if !ok {
// 		d = &Datum{}
// 		m.Values[key] = d
// 	}
// 	d.Inc(delta, timestamp)
// }

// func (m *Metric) SetD(keys []string, value int64, timestamp time.Time) {
// 	if len(m.Keys) == 0 {
// 		panic("can't set with keys a nondimensional metric")
// 	}
// 	key := key_hash(keys)
// 	d, ok := m.Values[key]
// 	if !ok {
// 		d = &Datum{}
// 		m.Values[key] = d
// 	}
// 	d.Set(value, timestamp)
// }

// func (m *Metric) Value() *Datum {
// 	if len(m.Keys) > 0 {
// 		panic("can't get a dimensional metric")
// 	}
// 	key := " " // Dirty Hack
// 	return m.Values[key]
// }

// func (m *Metric) ValueD(keys []string) *Datum {
// 	if len(m.Keys) > 0 {
// 		panic("can't get with keys a nondimensional metric")
// 	}
// 	key := key_hash(keys)
// 	return m.Values[key]
// }
