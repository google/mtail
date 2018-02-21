// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.
// +build !go1.7

package main

import "github.com/golang/glog"

func SetMutexProfileFraction(rate int) int {
	glog.Info("SetMutexProfileFraction not supported")
	return rate
}
