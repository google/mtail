// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

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

// MergePosition returns the union of two positions such that the result contains both inputs.
func MergePosition(a, b *Position) *Position {
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

// mergepositionlist is a helper that merges the positions of all the nodes in a list
func mergepositionlist(l []astNode) *Position {
	if len(l) == 0 {
		return nil
	}
	if len(l) == 1 {
		if l[0] != nil {
			return l[0].Pos()
		}
		return nil
	}
	return MergePosition(l[0].Pos(), mergepositionlist(l[1:]))
}
