// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

// StringDatum describes a string value at a given timestamp.
type StringDatum struct {
	BaseDatum
	mu    sync.RWMutex
	Value string
}

// Type returns the Type of an StringDatum, String.
func (*StringDatum) Type() Type { return String }

// Set sets the value of the StringDatum to the value at timestamp.
func (d *StringDatum) Set(value string, timestamp time.Time) {
	d.mu.Lock()
	d.Value = value
	d.stamp(timestamp)
	d.mu.Unlock()
}

// Get returns the value of the StringDatum
func (d *StringDatum) Get() string {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return d.Value
}

// String returns a string representation of the StringDatum.
func (d *StringDatum) String() string {
	return fmt.Sprintf("%q@%d", d.Get(), atomic.LoadInt64(&d.Time))
}

// ValueString returns the value of the StringDatum as a string.
func (d *StringDatum) ValueString() string {
	return d.Get()
}

// MarshalJSON returns a JSON encoding of the StringDatum.
func (d *StringDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value string
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
