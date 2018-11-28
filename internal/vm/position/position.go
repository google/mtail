// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package position

import "fmt"

// A Position is the location in the source program that a token appears.
type Position struct {
	Filename string // Source filename in which this token appears.
	Line     int    // Line in the source for this token.
	Startcol int    // Starting and ending columns in the source for this token.
	Endcol   int
}

func (p Position) String() string {
	r := fmt.Sprintf("%s:%d:%d", p.Filename, p.Line+1, p.Startcol+1)
	if p.Endcol > p.Startcol {
		r += fmt.Sprintf("-%d", p.Endcol+1)
	}
	return r
}
