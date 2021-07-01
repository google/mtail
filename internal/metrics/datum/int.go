// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"sync/atomic"
	"time"
)

// Int describes an integer value at a given timestamp.
type Int struct {
	BaseDatum
	Value int64
}

// Set sets the value of the Int to the value at timestamp.
func (d *Int) Set(value int64, timestamp time.Time) {
	atomic.StoreInt64(&d.Value, value)
	d.stamp(timestamp)
}

// IncBy increments the Int's value by the value provided, at timestamp.
func (d *Int) IncBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.Value, delta)
	d.stamp(timestamp)
}

// DecBy increments the Int's value by the value provided, at timestamp.
func (d *Int) DecBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.Value, -delta)
	d.stamp(timestamp)
}

// Get returns the value of the Int.
func (d *Int) Get() int64 {
	return atomic.LoadInt64(&d.Value)
}

// ValueString returns the value of the Int as a string.
func (d *Int) ValueString() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.Value))
}

// MarshalJSON returns a JSON encoding of the Int.
func (d *Int) MarshalJSON() ([]byte, error) {
	j := struct {
		Value int64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
