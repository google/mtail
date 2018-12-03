// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package bytecode_test

import (
	"testing"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/bytecode"
)

func TestBytecodeString(t *testing.T) {
	expected := "{match 0}"

	if diff := testutil.Diff(bytecode.Instr{bytecode.Match, 0}.String(), expected); diff != "" {
		t.Errorf("bytecodee string didn't match:\n%s", diff)
	}
}
