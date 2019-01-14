// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"flag"
	"testing"
)

// TestSetFlag sets the value of the commandline flag, and returns a cleanup function that restores the flag value.
func TestSetFlag(tb testing.TB, name, value string) func() {
	tb.Helper()
	val := flag.Lookup(name)

	flag.Set(name, value)
	flag.Parse()

	return func() {
		if val != nil {
			flag.Set(name, val.Value.String())
		}
	}
}
