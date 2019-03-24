// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"fmt"
	"math"
	"sort"
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
	// Buckets describes histograms
	Buckets
)

func (t Type) String() string {
	switch t {
	case Int:
		return "Int"
	case Float:
		return "Float"
	case String:
		return "String"
	case Buckets:
		return "Buckets"
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

	// Time returns the timestamp of the Datum as time.Time in UTC
	TimeUTC() time.Time
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

func (d *BaseDatum) TimeUTC() time.Time {
	tNsec := atomic.LoadInt64(&d.Time)
	return time.Unix(tNsec/1e9, tNsec%1e9)
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

// NewBuckets creates a new zero buckets datum.
func NewBuckets(buckets []Range) Datum {
	return MakeBuckets(buckets, zeroTime)
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

// MakeBuckets creates a new bucket datum with the provided list of ranges and
// timestamp.  If no +inf bucket is provided, one is created.
func MakeBuckets(buckets []Range, ts time.Time) Datum {
	d := &BucketsDatum{}
	seenInf := false
	highest := 0.0
	for _, b := range buckets {
		d.AddBucket(b)
		if math.IsInf(b.Max, +1) {
			seenInf = true
		} else if b.Max > highest {
			highest = b.Max
		}
	}
	if !seenInf {
		d.AddBucket(Range{highest, math.Inf(+1)})
	}
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
	case *BucketsDatum:
		d.Observe(float64(v), ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

// SetFloat sets a floating-point Datum to the provided value and timestamp, or panics if the Datum is not a FloatDatum.
func SetFloat(d Datum, v float64, ts time.Time) {
	switch d := d.(type) {
	case *FloatDatum:
		d.Set(v, ts)
	case *BucketsDatum:
		d.Observe(v, ts)
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

// DecIntBy increments an integer Datum by the provided value, at time ts, or panics if the Datum is not an IntDatum.
func DecIntBy(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *IntDatum:
		d.DecBy(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

func GetBuckets(d Datum) *BucketsDatum {
	switch d := d.(type) {
	case *BucketsDatum:
		return d
	default:
		panic(fmt.Sprintf("datum %v is not a Buckets", d))
	}
}

// Observe records an observation v at time ts in d, or panics if d is not a BucketsDatum
func Observe(d Datum, v float64, ts time.Time) {
	switch d := d.(type) {
	case *BucketsDatum:
		d.Observe(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not a Buckets", d))
	}
}

// GetBucketCount returns the total count of observations in d, or panics if d is not a BucketsDatum
func GetBucketsCount(d Datum) uint64 {
	switch d := d.(type) {
	case *BucketsDatum:
		return d.Count()
	default:
		panic(fmt.Sprintf("datum %v is not a Buckets", d))
	}
}

// GetBucketsSum returns the sum of observations in d, or panics if d is not a BucketsDatum
func GetBucketsSum(d Datum) float64 {
	switch d := d.(type) {
	case *BucketsDatum:
		return d.Sum()
	default:
		panic(fmt.Sprintf("datum %v is not a Buckets", d))
	}
}

// GetBucketsByMax returns a map of cumulative bucket observations by their
// upper bonds, or panics if d is not a BucketsDatum.
func GetBucketsByMax(d Datum) map[float64]uint64 {
	switch d := d.(type) {
	case *BucketsDatum:
		buckets := make(map[float64]uint64, 0)
		maxes := make([]float64, 0)
		for r, c := range d.Buckets() {
			maxes = append(maxes, r.Max)
			buckets[r.Max] = c
		}
		sort.Sort(sort.Float64Slice(maxes))
		cum := uint64(0)
		for _, m := range maxes {
			cum += buckets[m]
			buckets[m] = cum
		}
		return buckets
	default:
		panic(fmt.Sprintf("datum %v is not a Buckets", d))
	}
}
