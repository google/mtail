// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build !1.7 !1.8

package main

import "github.com/golang/glog"

func SetMutexProfileFraction(rate int) int {
	glog.Info("SetMutexProfileFraction not supported")
	return rate
}
