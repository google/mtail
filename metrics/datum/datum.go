// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package datum

import (
	"fmt"
	"time"
)

type Type int

const (
	Int Type = iota
	Float
)

// Datum
type Datum interface {
	// Type returns the Datum type.
	Type() Type

	Value() string

	Time() string
}

func NewInt() Datum {
	return MakeInt(0, time.Unix(0, 0))
}

func MakeInt(v int64, ts time.Time) Datum {
	d := &intDatum{}
	d.Set(v, ts)
	return d
}

func GetInt(d Datum) int64 {
	switch d := d.(type) {
	case *intDatum:
		return d.Get()
	default:
		panic(fmt.Sprintf("%v is not an Int", d))
	}
}

func SetInt(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *intDatum:
		d.Set(v, ts)
	default:
		panic(fmt.Sprintf("%v is not an Int", d))
	}
}

func IncIntBy(d Datum, v int64, ts time.Time) {
	switch d := d.(type) {
	case *intDatum:
		d.IncBy(v, ts)
	default:
		panic(fmt.Sprintf("%v is not an Int", d))
	}
}
