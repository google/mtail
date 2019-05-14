// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package metrics

// Type describes the type of value stored in a Datum.
type Type int

const (
	// Int indicates this metric is an integer metric type
	Int Type = iota
	// Float indicates this metric is a floating-point metric type.
	Float
	// String indicates this metric contains printable string values.
	String
	// Buckets indicates this metric is a histogram metric type.
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
