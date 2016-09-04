// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

type Type int

const (
	Untyped Type = iota // Untyped indicates no type has been determined
	None
	String
	Int
	Float
)
