// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build go1.7

package main

import "runtime"

func SetMutexProfileFraction(rate int) int {
	return runtime.SetMutexProfileFraction(rate)
}
