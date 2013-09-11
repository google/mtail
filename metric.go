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

type Node struct {
	D    *Datum
	Next map[string]*Node
}

type Metric struct {
	Name    string // Name
	Program string // Instantiating program
	Kind    metric_type
	Keys    []string
	Values  *Node
}

func NewMetric(name string, prog string, kind metric_type, keys ...string) *Metric {
	m := &Metric{Name: name, Program: prog, Kind: kind,
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

func (m *Metric) GetDatum(labelvalues ...string) (*Datum, error) {
	if len(labelvalues) > len(m.Keys) {
		return nil, errors.New(fmt.Sprintf("Label values requested (%q) longer than keys for metric %q", labelvalues, m))
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
	return n.D, nil
}

type LabelSet struct {
	labels map[string]string
	datum  *Datum
}

func emitFromNode(n *Node, keys []string, values []string, c chan *LabelSet) {
	if n.D != nil {
		c <- &LabelSet{zip(keys, values), n.D}
	}
	for l, n1 := range n.Next {
		v := append(values, l)
		emitFromNode(n1, keys, v, c)
	}
}

func zip(keys []string, values []string) map[string]string {
	r := make(map[string]string, 0)
	for i, v := range values {
		r[keys[i]] = v
	}
	return r
}

func (m *Metric) EmitLabelSets(c chan *LabelSet, quit chan bool) {
	emitFromNode(m.Values, m.Keys, []string{}, c)
	quit <- true
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

func init() {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	metrics = make([]*Metric, 0)
}

func ExportMetric(m *Metric) {
	metric_lock.Lock()
	defer metric_lock.Unlock()
	metrics = append(metrics, m)
}
