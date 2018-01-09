// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "fmt"

type compileError struct {
	pos position
	msg string
}

func (e compileError) Error() string {
	return e.pos.String() + ": " + e.msg
}

// ErrorList contains a list of compile errors.
type ErrorList []*compileError

// Add appends an error at a position to the list of errors.
func (p *ErrorList) Add(pos *position, msg string) {
	*p = append(*p, &compileError{*pos, msg})
}

func (p *ErrorList) Append(l ErrorList) {
	*p = append(*p, l...)
}

// ErrorList implements the error interface.
func (p ErrorList) Error() string {
	switch len(p) {
	case 0:
		return "no errors"
	case 1:
		return p[0].Error()
	}
	var r string
	for _, e := range p {
		r = r + fmt.Sprintf("%s\n", e)
	}
	return r[:len(r)-1]
}
