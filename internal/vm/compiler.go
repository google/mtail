// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"io"
	"path/filepath"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/parser"
)

// Compile compiles a program from the input into a virtual machine or a list
// of compile errors.  It takes the program's name and the metric store as
// additional arguments to build the virtual machine.
func Compile(name string, input io.Reader, emitAst bool, emitAstTypes bool, syslogUseCurrentYear bool, loc *time.Location) (*VM, error) {
	name = filepath.Base(name)

	ast, err := parser.Parse(name, input)
	if err != nil {
		return nil, err
	}
	if emitAst {
		s := parser.Sexp{}
		glog.Infof("%s AST:\n%s", name, s.Dump(ast))
	}

	if ast, err = checker.Check(ast); err != nil {
		return nil, err
	}
	if emitAstTypes {
		s := parser.Sexp{}
		s.EmitTypes = true
		glog.Infof("%s AST with Type Annotation:\n%s", name, s.Dump(ast))
	}

	obj, err := CodeGen(name, ast)
	if err != nil {
		return nil, err
	}

	vm := New(name, obj, syslogUseCurrentYear, loc)
	return vm, nil
}
