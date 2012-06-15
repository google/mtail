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

type Metric interface {
	Name() string
	Kind() metric_type
	IsExported() bool
}

type MetricBase struct {
	name     string
	kind     metric_type
	exported bool
}

type ScalarMetric struct {
	MetricBase
	d Datum
}

func (m *ScalarMetric) Name() string {
	return m.name
}

func (m *ScalarMetric) Kind() metric_type {
	return m.kind
}

func (m *ScalarMetric) IsExported() bool {
	return m.exported
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

func (m *DimensionedMetric) Name() string {
	return m.name
}

func (m *DimensionedMetric) Kind() metric_type {
	return m.kind
}

func (m *DimensionedMetric) IsExported() bool {
	return m.exported
}

type Datum struct {
	Value int64
	Time  time.Time
}

var (
	metric_lock sync.RWMutex
	metrics     []Metric
)

const KEY_HASH_SEP = " "

func key_hash(keys []string) string {
	return strings.Join(keys, KEY_HASH_SEP)
}

func key_unhash(key string) []string {
	return strings.Split(key, KEY_HASH_SEP)
}

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

func init() {
	metrics = make([]Metric, 0)
}
