// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

import (
	"fmt"
	"sync/atomic"
	"time"
)

type Type int

const (
	Int Type = iota
	Float
)

// Datum
type Datum interface {
	Type() Type
	Value() string
	Time(format string) string
}

// Datum describes a LabelSet's or LabelValue's value at a given timestamp.
type IntDatum struct {
	Value int64
	Time  int64 // nanoseconds since unix epoch
}

func (d *IntDatum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		atomic.StoreInt64(&d.Time, time.Now().UTC().UnixNano())
	} else {
		atomic.StoreInt64(&d.Time, timestamp.UnixNano())
	}
}

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
	return fmt.Sprintf("%v@%d", atomic.LoadInt64(&d.Value), atomic.LoadInt64(&d.Time))
}
