// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"testing"
)

func SkipIfShort(tb testing.TB) {
	tb.Helper()
	if testing.Short() {
		tb.Skip("skipping test in -short mode")
	}
}
