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

// floatDatum describes a floating point value at a given timestamp.
type floatDatum struct {
	datum
	valuebits uint64
}

func (*floatDatum) Type() Type { return Float }

func (d *floatDatum) Value() string {
	return fmt.Sprintf("%g", d.Get())
}

func (d *floatDatum) Set(v float64, ts time.Time) {
	atomic.StoreUint64(&d.valuebits, math.Float64bits(v))
	d.stamp(ts)
}

func (d *floatDatum) Get() float64 {
	return math.Float64frombits(atomic.LoadUint64(&d.valuebits))
}

func (d *floatDatum) String() string {
	return fmt.Sprintf("%g@%d", d.Get(), atomic.LoadInt64(&d.time))
}

func (d *floatDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value float64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.time)}
	return json.Marshal(j)
}
