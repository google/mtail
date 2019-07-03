// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package errors_test

import (
	"testing"

	"github.com/google/mtail/internal/vm/errors"
)

func TestNilErrorPosition(t *testing.T) {
	e := errors.ErrorList{}
	e.Add(nil, "error")
	r := e.Error()
	if r != "error" {
		t.Errorf("want 'error', got %q", r)
	}
}
