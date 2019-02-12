// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm_test

import (
	"strings"
	"testing"

	"github.com/google/mtail/internal/vm"
)

func TestCompileParserError(t *testing.T) {
	r := strings.NewReader("bad program")
	_, err := vm.Compile("test", r, true, true, true, nil)
	if err == nil {
		t.Errorf("expected error, got nil")
	}
}

func TestCompileCheckerError(t *testing.T) {
	r := strings.NewReader(`// {
i++
}`)
	_, err := vm.Compile("test", r, true, true, true, nil)
	if err == nil {
		t.Error("expected error, got nil")
	}
}

func TestCompileCodegen(t *testing.T) {
	r := strings.NewReader(`counter i
// {
  i++
}`)
	_, err := vm.Compile("test", r, true, true, true, nil)
	if err != nil {
		t.Error(err)
	}
}
