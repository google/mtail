// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// +build gofuzz

package vm

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"os"

	"github.com/google/mtail/internal/logline"
)

const SEP = "\u2424"

func Fuzz(data []byte) int {
	// Enable this when debugging with a fuzz crash artifact.
	dumpDebug := false

	offset := bytes.Index(data, []byte(SEP))
	if offset < 0 {
		offset = len(data)
		data = append(data, []byte(SEP)...)
	}
	// We need to successfully parse flags to initialize the glog logger used
	// by the compiler, but the fuzzer gets called with flags captured by the
	// libfuzzer main, which we don't want to intercept here.
	flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ContinueOnError)
	flag.CommandLine.Parse([]string{})
	v, err := Compile("fuzz", bytes.NewReader(data[:offset]), dumpDebug, dumpDebug, false, nil, 0, 0)
	if err != nil {
		if dumpDebug {
			fmt.Print(err)
		}
		return 0 // false
	}
	v.HardCrash = true
	scanner := bufio.NewScanner(bytes.NewBuffer(data[offset+2:]))
	for scanner.Scan() {
		v.ProcessLogLine(context.Background(), logline.New(context.Background(), "fuzz", scanner.Text()))
	}
	return 1
}
