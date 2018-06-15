// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"fmt"
	"sync/atomic"
	"time"
)

// Type describes the type of value stored in a Datum.
type Type int

const (
	// Int describes an integer datum
	Int Type = iota
	// Float describes a floating point datum
	Float
	// String describes printable strings of text
	String
)

func (t Type) String() string {
	switch t {
	case Int:
		return "Int"
	case Float:
		return "Float"
	case String:
		return "String"
	}
	return "?"
}

// Datum is an interface for metric datums, with a type, value and timestamp to be exported.
type Datum interface {
	// Type returns the Datum type.
	Type() Type

	// ValueString returns the value of a Datum as a string.
	ValueString() string

	// TimeString returns the timestamp of a Datum as a string.
	TimeString() string
}

// BaseDatum is a struct used to record timestamps across all Datum implementations.
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

// TimeString returns the timestamp of this Datum as a string.
func (d *BaseDatum) TimeString() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.Time)/1e9)
}

// NewInt creates a new zero integer datum.
func NewInt() Datum {
	return MakeInt(0, zeroTime)
}

// NewFloat creates a new zero floating-point datum.
func NewFloat() Datum {
	return MakeFloat(0., zeroTime)
}

// NewString creates a new zero string datum.
func NewString() Datum {
	return MakeString("", zeroTime)
}

// MakeInt creates a new integer datum with the provided value and timestamp.
func MakeInt(v int64, ts time.Time) Datum {
	d := &IntDatum{}
	d.Set(v, ts)
	return d
}

// MakeFloat creates a new floating-point datum with the provided value and timestamp.
func MakeFloat(v float64, ts time.Time) Datum {
	d := &FloatDatum{}
	d.Set(v, ts)
	return d
}

// MakeString creates a new string datum with the provided value and timestamp
func MakeString(v string, ts time.Time) Datum {
	d := &StringDatum{}
	d.Set(v, ts)
	return d
}

// GetInt returns the integer value of a datum, or error.
func GetInt(d Datum) int64 {
	switch d := d.(type) {
	case *IntDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

// GetFloat returns the floating-point value of a datum, or error.
func GetFloat(d Datum) float64 {
	switch d := d.(type) {
	case *FloatDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

// GetString returns the string of a datum, or error.
func GetString(d Datum) string {
	switch d := d.(type) {
	case *StringDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not a String", d))
	}
}

// SetInt sets an integer datum to the provided value and timestamp, or panics if the Datum is not an IntDatum.
func SetInt(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *IntDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

// SetFloat sets a floating-point Datum to the provided value and timestamp, or panics if the Datum is not a FloatDatum.
func SetFloat(d Datum, v float64, ts time.Time) {
	switch d := d.(type) {
	case *FloatDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

// SetString sets a string Datum to the provided value and timestamp, or panics if the Datym is not a String Datum
func SetString(d Datum, v string, ts time.Time) {
	switch d := d.(type) {
	case *StringDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not a String", d))
	}
}

// IncIntBy increments an integer Datum by the provided value, at time ts, or panics if the Datum is not an IntDatum.
func IncIntBy(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *IntDatum:
		d.IncBy(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}
