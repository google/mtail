// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"testing"

	"github.com/google/mtail/internal/testutil"
)

func TestBytecodeString(t *testing.T) {
	expected := "{match 0}"

	if diff := testutil.Diff(Instr{Match, 0}.String(), expected); diff != "" {
		t.Errorf("bytedoce string didn't match:\n%s", diff)
	}
}
