// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package errors

import (
	"fmt"
	"strings"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/pkg/errors"
)

type compileError struct {
	pos position.Position
	msg string
}

func (e compileError) Error() string {
	return e.pos.String() + ": " + e.msg
}

// ErrorList contains a list of compile errors.
type ErrorList []*compileError

// Add appends an error at a position to the list of errors.
func (p *ErrorList) Add(pos *position.Position, msg string) {
	if pos == nil {
		pos = &position.Position{"", -1, -1, -1}
	}
	*p = append(*p, &compileError{*pos, msg})
}

// Append puts an ErrorList on the end of this ErrorList.
func (p *ErrorList) Append(l ErrorList) {
	*p = append(*p, l...)
}

// ErrorList implements the error interface.
func (p *ErrorList) Error() string {
	switch len(*p) {
	case 0:
		return "no errors"
	case 1:
		return (*p)[0].Error()
	}
	var r strings.Builder
	for _, e := range *p {
		r.WriteString(fmt.Sprintf("%s\n", e))
	}
	return r.String()[:r.Len()-1]
}

func Errorf(format string, args ...interface{}) error {
	return errors.Errorf(format, args...)
}
