// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Reimport the go-cmp package as the name 'cmp' conflicts with the cmp
// instruction in the vm.
package testutil

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

func Diff(a, b interface{}, opts ...cmp.Option) string {
	return cmp.Diff(a, b, opts...)
}

func IgnoreUnexported(types ...interface{}) cmp.Option {
	return cmpopts.IgnoreUnexported(types...)
}

func AllowUnexported(types ...interface{}) cmp.Option {
	return cmp.AllowUnexported(types...)
}

func IgnoreFields(typ interface{}, names ...string) cmp.Option {
	return cmpopts.IgnoreFields(typ, names...)
}

func SortSlices(lessFunc interface{}) cmp.Option {
	return cmpopts.SortSlices(lessFunc)
}

// ExpectNoDiff tests to see if the two interfaces have no diff.
// If there is no diff, the retrun value is true.
// If there is a diff, it is logged to tb and an error is flagged, and the return value is false.
func ExpectNoDiff(tb testing.TB, a, b interface{}, opts ...cmp.Option) bool {
	tb.Helper()
	if diff := Diff(a, b, opts...); diff != "" {
		tb.Errorf("Unexpected diff, -want +got:\n%s", diff)
		tb.Logf("expected:\n%#v", a)
		tb.Logf("received:\n%#v", b)
		return false
	}
	return true
}
