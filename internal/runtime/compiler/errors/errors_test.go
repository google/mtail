// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package errors_test

import (
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/errors"
)

func TestNilErrorPosition(t *testing.T) {
	e := errors.ErrorList{}
	e.Add(nil, "error")
	r := e.Error()
	expected := ":0:0: error"
	if r != expected {
		t.Errorf("want %q, got %q", expected, r)
	}
}
