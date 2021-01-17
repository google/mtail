// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"flag"
	"testing"
)

// SetFlag sets the value of the commandline flag, and registers a cleanup function that restores the flag value.
func SetFlag(tb testing.TB, name, value string) {
	tb.Helper()
	val := flag.Lookup(name)

	if err := flag.Set(name, value); err != nil {
		tb.Fatal(err)
	}

	tb.Cleanup(func() {
		if val != nil {
			if err := flag.Set(name, val.Value.String()); err != nil {
				tb.Fatal(err)
			}
		}
	})
}
