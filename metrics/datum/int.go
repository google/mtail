// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"encoding/json"
	"fmt"
	"sync/atomic"
	"time"
)

// Datum describes a LabelSet's or LabelValue's value at a given timestamp.
type intDatum struct {
	value int64
	time  int64 // nanoseconds since unix epoch
}

func (*intDatum) Type() Type { return Int }

func (d *intDatum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		atomic.StoreInt64(&d.time, time.Now().UTC().UnixNano())
	} else {
		atomic.StoreInt64(&d.time, timestamp.UnixNano())
	}
}

func (d *intDatum) Time() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.time)/1e9)
}

// Set implements the Settable interface for a Datum.
func (d *intDatum) Set(value int64, timestamp time.Time) {
	atomic.StoreInt64(&d.value, value)
	d.stamp(timestamp)
}

// IncBy implements the Incrementable interface for a Datum.
func (d *intDatum) IncBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.value, delta)
	d.stamp(timestamp)
}

// Get returns the value of the Datum.
func (d *intDatum) Get() int64 {
	return atomic.LoadInt64(&d.value)
}

func (d *intDatum) String() string {
	return fmt.Sprintf("%v@%d", atomic.LoadInt64(&d.value), atomic.LoadInt64(&d.time))
}

func (d *intDatum) Value() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.value))
}

func (d *intDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value int64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.time)}
	return json.Marshal(j)
}
