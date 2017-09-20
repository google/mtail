// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

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

func (t Type) String() string {
	switch t {
	case Int:
		return "Int"
	case Float:
		return "Float"
	}
	return "?"
}

// Datum
type Datum interface {
	// Type returns the Datum type.
	Type() Type

	ValueString() string

	TimeString() string
}

type BaseDatum struct {
	Time int64 // nanoseconds since unix epoch
}

var zeroTime time.Time

func (d *BaseDatum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		atomic.StoreInt64(&d.Time, time.Now().UTC().UnixNano())
	} else {
		atomic.StoreInt64(&d.Time, timestamp.UnixNano())
	}
}

func (d *BaseDatum) TimeString() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.Time)/1e9)
}

func NewInt() Datum {
	return MakeInt(0, zeroTime)
}

func NewFloat() Datum {
	return MakeFloat(0., zeroTime)
}

func MakeInt(v int64, ts time.Time) Datum {
	d := &IntDatum{}
	d.Set(v, ts)
	return d
}

func MakeFloat(v float64, ts time.Time) Datum {
	d := &FloatDatum{}
	d.Set(v, ts)
	return d
}

func GetInt(d Datum) int64 {
	switch d := d.(type) {
	case *IntDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

func GetFloat(d Datum) float64 {
	switch d := d.(type) {
	case *FloatDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

func SetInt(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *IntDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

func SetFloat(d Datum, v float64, ts time.Time) {
	switch d := d.(type) {
	case *FloatDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

func IncIntBy(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *IntDatum:
		d.IncBy(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}
