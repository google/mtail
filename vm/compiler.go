// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Build the parser:
//go:generate $GOPATH/bin/goyacc -v y.output -o parser.go -p mtail parser.y

package vm

import (
	"io"
	"path/filepath"
	"regexp"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

// Options contains all the parameters that affect the behaviour of the compiler.
type Options struct {
	CompileOnly          bool           // Do not start the program after compilation.
	SyslogUseCurrentYear bool           // Use the current year if no year is present in the log file timestamp.
	OverrideLocation     *time.Location //
	EmitAst              bool           // Print the AST after parse
	EmitAstTypes         bool           // Print the AST with types after typechecking
}

// Compile compiles a program from the input into a virtual machine or a list
// of compile errors.  It takes the program's name and the metric store as
// additional arguments to build the virtual machine.
func Compile(name string, input io.Reader, o *Options) (*VM, error) {
	name = filepath.Base(name)

	ast, err := Parse(name, input)
	if err != nil {
		return nil, err
	}
	if o.EmitAst {
		s := Sexp{}
		glog.Infof("%s AST:\n%s", name, s.Dump(ast))
	}

	if err := Check(ast); err != nil {
		return nil, err
	}
	if o.EmitAstTypes {
		s := Sexp{}
		s.emitTypes = true
		glog.Infof("%s AST with Type Annotation:\n%s", name, s.Dump(ast))
	}

	obj, err := CodeGen(name, ast)
	if err != nil {
		return nil, err
	}

	vm := New(name, obj, o.SyslogUseCurrentYear, o.OverrideLocation)
	return vm, nil
}

// object describes a built object of data and bytecode
type object struct {
	prog []instr           // The emitted program.
	str  []string          // Static strings.
	re   []*regexp.Regexp  // Static regular expressions.
	m    []*metrics.Metric // Metrics accessible to this program.
}
