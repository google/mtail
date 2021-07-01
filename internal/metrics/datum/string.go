// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"sync"
	"sync/atomic"
	"time"
)

// String describes a string value at a given timestamp.
type String struct {
	BaseDatum
	mu    sync.RWMutex
	Value string
}

// Set sets the value of the String to the value at timestamp.
func (d *String) Set(value string, timestamp time.Time) {
	d.mu.Lock()
	d.Value = value
	d.stamp(timestamp)
	d.mu.Unlock()
}

// Get returns the value of the String.
func (d *String) Get() string {
	d.mu.RLock()
	defer d.mu.RUnlock()
	return d.Value
}

// ValueString returns the value of the String as a string.
func (d *String) ValueString() string {
	return d.Get()
}

// MarshalJSON returns a JSON encoding of the String.
func (d *String) MarshalJSON() ([]byte, error) {
	j := struct {
		Value string
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
