// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package metrics provides storage for metrics being recorded by mtail
// programs.
package metrics

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"reflect"
	"strings"
	"sync"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/pkg/errors"
)

// Kind enumerates the types of metrics supported.
type Kind int

const (
	_ Kind = iota

	// Counter is a monotonically nondecreasing metric.
	Counter

	// Gauge is a Kind that can take on any value, and may be set
	// discontinuously from its previous value.
	Gauge

	// Timer is a specialisation of Gauge that can be used to store time
	// intervals, such as latency and durations.  It enables certain behaviour
	// in exporters that handle time intervals such as StatsD.
	Timer

	// Text is a special metric type for free text, usually for operating as a 'hidden' metric, as often these values cannot be exported.
	Text

	// Histogram is a Kind that observes a value and stores the value
	// in a bucket.
	Histogram

	endKind // end of enumeration for testing
)

func (m Kind) String() string {
	switch m {
	case Counter:
		return "Counter"
	case Gauge:
		return "Gauge"
	case Timer:
		return "Timer"
	case Text:
		return "Text"
	case Histogram:
		return "Histogram"
	}
	return "Unknown"
}

// Generate implements the quick.Generator interface for Kind.
func (Kind) Generate(rand *rand.Rand, size int) reflect.Value {
	return reflect.ValueOf(Kind(rand.Intn(int(endKind))))
}

// LabelValue is an object that names a Datum value with a list of label
// strings.
type LabelValue struct {
	Labels []string `json:",omitempty"`
	Value  datum.Datum
	// After this time of inactivity, the LabelValue is removed from the metric.
	Expiry time.Duration `json:",omitempty"`
}

// Metric is an object that describes a metric, with its name, the creator and
// owner program name, its Kind, a sequence of Keys that may be used to
// add dimension to the metric, and a list of LabelValues that contain data for
// labels in each dimension of the Keys.
type Metric struct {
	sync.RWMutex
	Name           string // Name
	Program        string // Instantiating program
	Kind           Kind
	Type           Type
	Hidden         bool          `json:",omitempty"`
	Keys           []string      `json:",omitempty"`
	LabelValues    []*LabelValue `json:",omitempty"`
	labelValuesMap map[string]*LabelValue
	Source         string        `json:",omitempty"`
	Buckets        []datum.Range `json:",omitempty"`
	Limit          int           `json:",omitempty"`
}

// NewMetric returns a new empty metric of dimension len(keys).
func NewMetric(name string, prog string, kind Kind, typ Type, keys ...string) *Metric {
	m := newMetric(len(keys))
	m.Name = name
	m.Program = prog
	m.Kind = kind
	m.Type = typ
	copy(m.Keys, keys)
	return m
}

// newMetric returns a new empty Metric.
func newMetric(keyLen int) *Metric {
	return &Metric{
		Keys:           make([]string, keyLen),
		LabelValues:    make([]*LabelValue, 0),
		labelValuesMap: make(map[string]*LabelValue),
	}
}

// buildLabelValueKey returns a unique key for the given labels.
func buildLabelValueKey(labels []string) string {
	var buf strings.Builder
	for i := 0; i < len(labels); i++ {
		rs := strings.ReplaceAll(labels[i], "-", "\\-")
		buf.WriteString(rs)
		buf.WriteString("-")
	}
	return buf.String()
}

func (m *Metric) AppendLabelValue(lv *LabelValue) error {
	if len(lv.Labels) != len(m.Keys) {
		return errors.Errorf("Label values requested (%q) not same length as keys for metric %v", lv.Labels, m)
	}
	m.LabelValues = append(m.LabelValues, lv)
	k := buildLabelValueKey(lv.Labels)
	m.labelValuesMap[k] = lv
	return nil
}

func (m *Metric) FindLabelValueOrNil(labelvalues []string) *LabelValue {
	k := buildLabelValueKey(labelvalues)
	lv, ok := m.labelValuesMap[k]
	if ok {
		return lv
	}
	return nil
}

// GetDatum returns the datum named by a sequence of string label values from a
// Metric.  If the sequence of label values does not yet exist, it is created.
func (m *Metric) GetDatum(labelvalues ...string) (d datum.Datum, err error) {
	if len(labelvalues) != len(m.Keys) {
		return nil, errors.Errorf("Label values requested (%q) not same length as keys for metric %v", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
	if lv := m.FindLabelValueOrNil(labelvalues); lv != nil {
		d = lv.Value
	} else {
		// TODO Check m.Limit and expire old data
		switch m.Type {
		case Int:
			d = datum.NewInt()
		case Float:
			d = datum.NewFloat()
		case String:
			d = datum.NewString()
		case Buckets:
			buckets := m.Buckets
			if buckets == nil {
				buckets = make([]datum.Range, 0)
			}
			d = datum.NewBuckets(buckets)
		}
		lv := &LabelValue{Labels: labelvalues, Value: d}
		if err := m.AppendLabelValue(lv); err != nil {
			return nil, err
		}
	}
	return d, nil
}

// Clear the "gause" data since they're not accumulated after reporting.
func (m *Metric) ClearGaugeData() {
	m.Lock()
	defer m.Unlock()
	m.ClearGaugeDataLocked()
}

func (m *Metric) ClearGaugeDataLocked() {
	if m.Kind == Gauge  {
		m.labelValuesMap = make(map[string]*LabelValue)
		m.LabelValues    = make([]*LabelValue, 0)
	}
}

// RemoveOldestDatum scans the Metric's LabelValues for the Datum with the oldest timestamp, and removes it.
func (m *Metric) RemoveOldestDatum() {
	var oldestLV *LabelValue
	for _, lv := range m.LabelValues {
		if oldestLV == nil || lv.Value.TimeUTC().Before(oldestLV.Value.TimeUTC()) {
			oldestLV = lv
		}
	}
	if oldestLV != nil {
		glog.V(1).Infof("removeOldest: removing oldest LV: %v", oldestLV)
		err := m.RemoveDatum(oldestLV.Labels...)
		if err != nil {
			glog.Warning(err)
		}
	}
}

// RemoveDatum removes the Datum described by labelvalues from the Metric m.
func (m *Metric) RemoveDatum(labelvalues ...string) error {
	if len(labelvalues) != len(m.Keys) {
		return errors.Errorf("Label values requested (%q) not same length as keys for metric %v", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
	k := buildLabelValueKey(labelvalues)
	olv, ok := m.labelValuesMap[k]
	if ok {
		for i := 0; i < len(m.LabelValues); i++ {
			lv := m.LabelValues[i]
			if lv == olv {
				// remove from the slice
				m.LabelValues = append(m.LabelValues[:i], m.LabelValues[i+1:]...)
				delete(m.labelValuesMap, k)
				break
			}
		}
	}
	return nil
}

func (m *Metric) ExpireDatum(expiry time.Duration, labelvalues ...string) error {
	if len(labelvalues) != len(m.Keys) {
		return errors.Errorf("Label values requested (%q) not same length as keys for metric %v", labelvalues, m)
	}
	m.Lock()
	defer m.Unlock()
	if lv := m.FindLabelValueOrNil(labelvalues); lv != nil {
		lv.Expiry = expiry
		return nil
	}
	return errors.Errorf("No datum for given labelvalues %q", labelvalues)
}

// LabelSet is an object that maps the keys of a Metric to the labels naming a
// Datum, for use when enumerating Datums from a Metric.
type LabelSet struct {
	Labels map[string]string
	Datum  datum.Datum
}

func zip(keys []string, values []string) map[string]string {
	r := make(map[string]string)
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

// UnmarshalJSON converts a JSON byte string into a LabelValue.
func (lv *LabelValue) UnmarshalJSON(b []byte) error {
	var obj map[string]*json.RawMessage
	err := json.Unmarshal(b, &obj)
	if err != nil {
		return err
	}

	labels := make([]string, 0)
	if _, ok := obj["Labels"]; ok {
		err = json.Unmarshal(*obj["Labels"], &labels)
		if err != nil {
			return err
		}
	}
	lv.Labels = labels

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
	return fmt.Sprintf("Metric: name=%s program=%s kind=%v type=%s hidden=%v keys=%v labelvalues=%v source=%s buckets=%v", m.Name, m.Program, m.Kind, m.Type, m.Hidden, m.Keys, m.LabelValues, m.Source, m.Buckets)
}

// SetSource sets the source of a metric, describing where in user programmes it was defined.
func (m *Metric) SetSource(source string) {
	m.Lock()
	defer m.Unlock()
	m.Source = source
}
