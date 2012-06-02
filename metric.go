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

func (m metric_type) String() string {
	switch m {
	case Counter:
		return "Counter"
	case Gauge:
		return "Gauge"
	}
	return "Unknown"
}

type Metric struct {
	Name  string
	Value int64
	Time  time.Time
	Type  metric_type
	Unit  string
	Tags  map[string]string
}

// Global metrics storage.
var (
	metric_lock sync.RWMutex
	metrics     []*Metric
)

func (m *Metric) Set(value int64, timestamp time.Time) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	m.Value = value
	if timestamp.IsZero() {
		m.Time = time.Now()
	} else {
		m.Time = timestamp
	}
}

func (m *Metric) Inc(delta int64, timestamp time.Time) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	m.Value += delta
	if timestamp.IsZero() {
		m.Time = time.Now()
	} else {
		m.Time = timestamp
	}
}
