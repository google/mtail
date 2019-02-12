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
