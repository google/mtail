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

// FloatDatum describes a floating point value at a given timestamp.
type FloatDatum struct {
	BaseDatum
	Valuebits uint64
}

// Type returns the Type of this Datum.
func (*FloatDatum) Type() Type { return Float }

// ValueString returns the value of the FloatDatum as a string.
func (d *FloatDatum) ValueString() string {
	return fmt.Sprintf("%g", d.Get())
}

// Set sets value of the FloatDatum at the timestamp ts.
func (d *FloatDatum) Set(v float64, ts time.Time) {
	atomic.StoreUint64(&d.Valuebits, math.Float64bits(v))
	d.stamp(ts)
}

// Get returns the floating-point value.
func (d *FloatDatum) Get() float64 {
	return math.Float64frombits(atomic.LoadUint64(&d.Valuebits))
}

// String returns a string representation of the FloatDatum.
func (d *FloatDatum) String() string {
	return fmt.Sprintf("%g@%d", d.Get(), atomic.LoadInt64(&d.Time))
}

// MarshalJSON returns a JSON encoding of the FloatDatum.
func (d *FloatDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value float64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
