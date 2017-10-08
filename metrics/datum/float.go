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
type FloatDatum struct {
	BaseDatum
	Valuebits uint64
}

func (*FloatDatum) Type() Type { return Float }

func (d *FloatDatum) ValueString() string {
	return fmt.Sprintf("%g", d.Get())
}

func (d *FloatDatum) Set(v float64, ts time.Time) {
	atomic.StoreUint64(&d.Valuebits, math.Float64bits(v))
	d.stamp(ts)
}

func (d *FloatDatum) Get() float64 {
	return math.Float64frombits(atomic.LoadUint64(&d.Valuebits))
}

func (d *FloatDatum) String() string {
	return fmt.Sprintf("%g@%d", d.Get(), atomic.LoadInt64(&d.Time))
}

func (d *FloatDatum) MarshalJSON() ([]byte, error) {
	j := struct {
		Value float64
		Time  int64
	}{d.Get(), atomic.LoadInt64(&d.Time)}
	return json.Marshal(j)
}
