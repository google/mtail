// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// +build gofuzz

package vm

import (
	"bytes"
	"flag"
)

func Fuzz(data []byte) int {
	flag.Set("logtostderr", "true")
	flag.Set("v", "2")
	flag.Parse()
	if _, err := Compile("fuzz", bytes.NewReader(data), false, false, false, nil); err != nil {
		return 0
	}
	return 1
}
