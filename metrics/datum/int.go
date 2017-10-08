// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"sync/atomic"
	"time"
)

// IntDatum describes an integer value at a given timestamp.
type IntDatum struct {
	BaseDatum
	Value int64
}

func (*IntDatum) Type() Type { return Int }

// Set implements the Settable interface for a Datum.
func (d *IntDatum) Set(value int64, timestamp time.Time) {
	atomic.StoreInt64(&d.Value, value)
	d.stamp(timestamp)
}

// IncBy implements the Incrementable interface for a Datum.
func (d *IntDatum) IncBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.Value, delta)
	d.stamp(timestamp)
}

// Get returns the value of the Datum.
func (d *IntDatum) Get() int64 {
	return atomic.LoadInt64(&d.Value)
}

func (d *IntDatum) String() string {
	return fmt.Sprintf("%d@%d", atomic.LoadInt64(&d.Value), atomic.LoadInt64(&d.Time))
}

func (d *IntDatum) ValueString() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.Value))
}

func (d *IntDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value int64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
