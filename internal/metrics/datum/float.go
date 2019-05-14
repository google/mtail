// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"math"
	"sync/atomic"
	"time"
)

// Float describes a floating point value at a given timestamp.
type Float struct {
	BaseDatum
	Valuebits uint64
}

// ValueString returns the value of the Float as a string.
func (d *Float) ValueString() string {
	return fmt.Sprintf("%g", d.Get())
}

// Set sets value of the Float at the timestamp ts.
func (d *Float) Set(v float64, ts time.Time) {
	atomic.StoreUint64(&d.Valuebits, math.Float64bits(v))
	d.stamp(ts)
}

// Get returns the floating-point value.
func (d *Float) Get() float64 {
	return math.Float64frombits(atomic.LoadUint64(&d.Valuebits))
}

// MarshalJSON returns a JSON encoding of the Float.
func (d *Float) MarshalJSON() ([]byte, error) {
	j := struct {
		Value float64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
