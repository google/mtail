// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package code

import "testing"

func TestOpcodeHasString(t *testing.T) {
	for o := Bad; o < lastOpcode; o++ {
		if o.String() != opNames[o] {
			t.Errorf("opcode string not match.  Expected %s, received %s", opNames[o], o.String())
		}
	}
}
