package testutil

import "testing"

// FatalIfErr fails the test with a fatal error if err is not nil.
func FatalIfErr(tb testing.TB, err error) {
	tb.Helper()
	if err != nil {
		tb.Fatal(err)
	}
}
