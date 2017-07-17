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
	Buckets
)

func (t Type) String() string {
	switch t {
	case Int:
		return "Int"
	case Float:
		return "Float"
	case Buckets:
		return "Buckets"
	}
	return "?"
}

// Datum
type Datum interface {
	// Type returns the Datum type.
	Type() Type

	Value() string

	Time() string
}

type datum struct {
	time int64 // nanoseconds since unix epoch
}

var zeroTime time.Time

func (d *datum) stamp(timestamp time.Time) {
	if timestamp.IsZero() {
		atomic.StoreInt64(&d.time, time.Now().UTC().UnixNano())
	} else {
		atomic.StoreInt64(&d.time, timestamp.UnixNano())
	}
}

func (d *datum) Time() string {
	return fmt.Sprintf("%d", atomic.LoadInt64(&d.time)/1e9)
}

func NewInt() Datum {
	return MakeInt(0, zeroTime)
}

func NewFloat() Datum {
	return MakeFloat(0., zeroTime)
}

func NewBuckets(buckets []Range) Datum {
	return MakeBuckets(0., buckets, zeroTime)
}

func MakeInt(v int64, ts time.Time) Datum {
	d := &intDatum{}
	d.Set(v, ts)
	return d
}

func MakeFloat(v float64, ts time.Time) Datum {
	d := &floatDatum{}
	d.Set(v, ts)
	return d
}

func MakeBuckets(v float64, buckets []Range, ts time.Time) Datum {
	d := &BucketsDatum{}

	for _, r := range buckets {
		d.AddBucket(r)
	}

	d.Set(v, ts)
	return d
}

func GetInt(d Datum) int64 {
	switch d := d.(type) {
	case *intDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

func GetFloat(d Datum) float64 {
	switch d := d.(type) {
	case *floatDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

func SetInt(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *intDatum:
		d.Set(v, ts)
	case *BucketsDatum:
		d.Set(float64(v), ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}

func SetFloat(d Datum, v float64, ts time.Time) {
	switch d := d.(type) {
	case *floatDatum:
		d.Set(v, ts)
	case *BucketsDatum:
		d.Set(float64(v), ts)
	default:
		panic(fmt.Sprintf("datum %v is not a Float", d))
	}
}

func IncIntBy(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *intDatum:
		d.IncBy(v, ts)
	default:
		panic(fmt.Sprintf("datum %v is not an Int", d))
	}
}
