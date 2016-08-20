// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Build the parser:
//go:generate goyacc -v y.output -o parser.go -p mtail parser.y

package vm

import (
	"io"
	"path/filepath"
	"regexp"

	"github.com/google/mtail/metrics"
)

// Options contains all the parameters that affect the behaviour of the compiler.
type Options struct {
	CompileOnly          bool // Do not start the program after compilation.
	SyslogUseCurrentYear bool // Use the current year if no year is present in the log file timestamp.
}

// Compile compiles a program from the input into a virtual machine or a list
// of compile errors.  It takes the program's name and the metric store as
// additional arguments to build the virtual machine.
func Compile(name string, input io.Reader, ms *metrics.Store, o *Options) (*VM, error) {
	name = filepath.Base(name)

	ast, err := Parse(name, input, ms)
	if err != nil {
		return nil, err
	}

	if err := Check(ast); err != nil {
		return nil, err
	}

	obj, err := CodeGen(name, ast)
	if err != nil {
		return nil, err
	}

	if o.CompileOnly {
		return nil, nil
	}

	vm := New(name, obj, o.SyslogUseCurrentYear)
	return vm, nil
}

// object describes a built object of data and bytecode
type object struct {
	prog []instr           // The emitted program.
	str  []string          // Static strings.
	re   []*regexp.Regexp  // Static regular expressions.
	m    []*metrics.Metric // Metrics accessible to this program.
}
