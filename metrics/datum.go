// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"fmt"
	"sync/atomic"
	"time"
)

// Datum describes a LabelSet's or LabelValue's value at a given timestamp.
type Datum struct {
	Value int64
	Time  int64 // nanoseconds since unix epoch
}

func (d *Datum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		atomic.StoreInt64(&d.Time, time.Now().UTC().UnixNano())
	} else {
		atomic.StoreInt64(&d.Time, timestamp.UnixNano())
	}
}

// Set implements the Settable interface for a Datum.
func (d *Datum) Set(value int64, timestamp time.Time) {
	atomic.StoreInt64(&d.Value, value)
	d.stamp(timestamp)
}

// IncBy implements the Incrementable interface for a Datum.
func (d *Datum) IncBy(delta int64, timestamp time.Time) {
	atomic.AddInt64(&d.Value, delta)
	d.stamp(timestamp)
}

// Get returns the value of the Datum.
func (d *Datum) Get() int64 {
	return atomic.LoadInt64(&d.Value)
}

func (d *Datum) String() string {
	return fmt.Sprintf("%v@%d", atomic.LoadInt64(&d.Value), atomic.LoadInt64(&d.Time))
}
