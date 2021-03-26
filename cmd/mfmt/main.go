// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

/*
Command mfmt formats mtail programs.
*/
package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/parser"
)

var (
	prog  = flag.String("prog", "", "Name of the mtail program text to format.")
	write = flag.Bool("write", false, "Write results to original file.")
)

func main() {
	flag.Parse()

	if *prog == "" {
		glog.Exitf("No -prog given")
	}

	f, err := os.OpenFile(*prog, os.O_RDWR, 0)
	if err != nil {
		glog.Exit(err)
	}
	ast, err := parser.Parse(*prog, f)
	if err != nil {
		glog.Exit(err)
	}
	ast, err = checker.Check(ast, 0, 0)
	if err != nil {
		glog.Exit(err)
	}
	up := parser.Unparser{}
	out := up.Unparse(ast)
	if *write {
		f.Truncate(0)
		f.Seek(0, io.SeekStart)
		f.WriteString(out)
	} else {

		fmt.Print(out)
	}
}
