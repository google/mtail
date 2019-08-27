// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// +build gofuzz

package vm

import (
	"bytes"
	"flag"
)

func Fuzz(data []byte) int {
	// We need to successfully parse flags to initialize the glog logger used
	// by the compiler, but the fuzzer gets called with flags captured by the
	// libfuzzer main, which we don't want to intercept here.
	flag.CommandLine = flag.NewFlagSet("", flag.ContinueOnError)
	if _, err := Compile("fuzz", bytes.NewReader(data), false, false, false, nil); err != nil {
		return 0
	}
	return 1
}
