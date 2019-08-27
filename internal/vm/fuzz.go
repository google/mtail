// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// +build gofuzz

package vm

import (
	"bytes"
	"flag"
	"fmt"
	"os"
)

func Fuzz(data []byte) int {
	// Enable this when debugging with a fuzz crash artifact.
	dumpDebug := false
	// We need to successfully parse flags to initialize the glog logger used
	// by the compiler, but the fuzzer gets called with flags captured by the
	// libfuzzer main, which we don't want to intercept here.
	flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ContinueOnError)
	flag.CommandLine.Parse([]string{})
	if _, err := Compile("fuzz", bytes.NewReader(data), dumpDebug, dumpDebug, false, nil); err != nil {
		if dumpDebug {
			fmt.Print(err)
		}
		return 0
	}
	return 1
}
