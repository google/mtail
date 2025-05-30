// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package code_test

import (
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/code"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func TestInstrString(t *testing.T) {
	expected := "{match 0 0}"

	testutil.ExpectNoDiff(t, code.Instr{Opcode: code.Match, Operand: 0}.String(), expected)
}
