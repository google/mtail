// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package position implements a data structure for storing source code positions.
package position

import "fmt"

// A Position is the location in the source program that a token appears.  It
// can specify a single character in the pinput, in which case the start and
// end columns are the same, or a span of sequential characters on one line.
type Position struct {
	Filename string // Source filename in which this token appears.
	Line     int    // Line in the source for this token.
	Startcol int    // Starting and ending columns in the source for this token.
	Endcol   int
}

// String formats a position to be useful for printing messages associated with
// this position, e.g. compiler errors.
func (p Position) String() string {
	r := fmt.Sprintf("%s:%d:%d", p.Filename, p.Line+1, p.Startcol+1)
	if p.Endcol > p.Startcol {
		r += fmt.Sprintf("-%d", p.Endcol+1)
	}
	return r
}

// MergePosition returns the union of two positions such that the result contains both inputs.
func Merge(a, b *Position) *Position {
	if a == nil {
		return b
	}
	if b == nil {
		return a
	}
	if a.Filename != b.Filename {
		return a
	}
	// TODO(jaq): handle multi-line positions
	if a.Line != b.Line {
		return a
	}
	r := *a
	if b.Startcol < r.Startcol {
		r.Startcol = b.Startcol
	}
	if b.Endcol > r.Endcol {
		r.Endcol = b.Endcol
	}
	return &r
}
