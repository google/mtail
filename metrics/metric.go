// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package metrics provides storage for metrics being recorded by mtail
// programs.
package metrics

import (
	"encoding/json"
	"fmt"
	"sync"
	"time"

	"github.com/google/mtail/metrics/datum"
)

// // Counter is a monotonically nondecreasing metric.
// type Counter interface {
// 	IncBy(delta int64, ts time.Time)
// }

// // Gauge is a non-monotonic metric.
// type Gauge interface {
// 	Set(value int64, ts time.Time)
// }

// Kind enumerates the types of metrics supported.
type Kind int

const (
	_ Kind = iota
	Counter
	// Gauge is a Kind that can take on any value, and may be set
	// discontinuously from its previous value.
	Gauge
	// Timer is a specialisation of Gauge that can be used to store time
	// intervals, such as latency and durations.  It enables certain behaviour
	// in exporters that handle time intervals such as StatsD.
	Timer
)

const (
	Int   = datum.Int
	Float = datum.Float
)

func (m Kind) String() string {
	switch m {
	case Counter:
		return "Counter"
	case Gauge:
		return "Gauge"
	case Timer:
		return "Timer"
	}
	return "Unknown"
}

// Incrementable describes an interface for Counter Kinds, that must be
// nondecreasing.
type Incrementable interface {
	IncBy(delta int64, ts time.Time)
}

// Settable describes an interface for Gauge Kinds, that can be set to
// any value discontinuously from its previous.
type Settable interface {
	Set(value int64, ts time.Time)
}

// LabelValue is an object that names a Datum value with a list of label
// strings.
type LabelValue struct {
	Labels []string `json:",omitempty"`
	Value  datum.Datum
}

// Metric is an object that describes a metric, with its name, the creator and
// owner program name, its Kind, a sequence of Keys that may be used to
// add dimension to the metric, and a list of LabelValues that contain data for
// labels in each dimension of the Keys.
type Metric struct {
	sync.RWMutex
	Name        string // Name
	Program     string // Instantiating program
	Kind        Kind
	Type        datum.Type
	Hidden      bool          `json:",omitempty"`
	Keys        []string      `json:",omitempty"`
	LabelValues []*LabelValue `json:",omitempty"`
}

// NewMetric returns a new empty metric of dimension len(keys).
func NewMetric(name string, prog string, kind Kind, typ datum.Type, keys ...string) *Metric {
	m := &Metric{Name: name, Program: prog, Kind: kind, Type: typ,
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
func (m *Metric) GetDatum(labelvalues ...string) (d datum.Datum, err error) {
	if len(labelvalues) != len(m.Keys) {
		return nil, fmt.Errorf("Label values requested (%q) not same length as keys for metric %q", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
	if lv := m.findLabelValueOrNil(labelvalues); lv != nil {
		d = lv.Value
	} else {
		switch m.Type {
		case datum.Int:
			d = datum.NewInt()
		case datum.Float:
			d = datum.NewFloat()
		}
		m.LabelValues = append(m.LabelValues, &LabelValue{labelvalues, d})
	}
	return d, nil
}

func (m *Metric) RemoveDatum(labelvalues ...string) error {
	if len(labelvalues) != len(m.Keys) {
		return fmt.Errorf("Label values requested (%q) not same length as keys for metric %q", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
Loop:
	for i, lv := range m.LabelValues {
		for j := 0; j < len(lv.Labels); j++ {
			if lv.Labels[j] != labelvalues[j] {
				continue Loop
			}
		}
		// remove from the slice
		m.LabelValues = append(m.LabelValues[:i], m.LabelValues[i+1:]...)
	}
	return nil
}

// LabelSet is an object that maps the keys of a Metric to the labels naming a
// Datum, for use when enumerating Datums from a Metric.
type LabelSet struct {
	Labels map[string]string
	Datum  datum.Datum
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

func (lv *LabelValue) String() string {
	return fmt.Sprintf("%v", *lv)
}

func (lv *LabelValue) UnmarshalJSON(b []byte) error {
	var obj map[string]*json.RawMessage
	err := json.Unmarshal(b, &obj)
	if err != nil {
		return err
	}

	var labels []string
	if _, ok := obj["Labels"]; ok {
		err = json.Unmarshal(*obj["Labels"], &labels)
		if err != nil {
			return err
		}
		lv.Labels = labels
	}

	var valObj map[string]*json.RawMessage
	err = json.Unmarshal(*obj["Value"], &valObj)
	if err != nil {
		return err
	}
	var t int64
	err = json.Unmarshal(*valObj["Time"], &t)
	if err != nil {
		return err
	}
	var i int64
	err = json.Unmarshal(*valObj["Value"], &i)
	if err != nil {
		return err
	}
	lv.Value = datum.MakeInt(i, time.Unix(t/1e9, t%1e9))
	return nil
}

func (m *Metric) String() string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%v", *m)
}
