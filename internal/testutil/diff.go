// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Reimport the go-cmp package as the name 'cmp' conflicts with the cmp
// instruction in the vm.
package testutil

import (
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
