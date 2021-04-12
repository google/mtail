// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"io"
	"path/filepath"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/codegen"
	"github.com/google/mtail/internal/vm/parser"
)

// Compile compiles a program from the input into a virtual machine or a list
// of compile errors.
func Compile(name string, input io.Reader, emitAst bool, emitAstTypes bool, syslogUseCurrentYear bool, loc *time.Location, maxRegexpLength int, maxRecursionDepth int) (*VM, error) { // TODO this is a prime candidate for Options pattern. See https://github.com/google/mtail/pull/474#discussion_r598044460
	name = filepath.Base(name)

	ast, err := parser.Parse(name, input)
	if err != nil {
		return nil, err
	}
	if emitAst {
		s := parser.Sexp{}
		glog.Infof("%s AST:\n%s", name, s.Dump(ast))
	}

	ast, err = checker.Check(ast, maxRegexpLength, maxRecursionDepth)
	if err != nil {
		return nil, err
	}
	if emitAstTypes {
		s := parser.Sexp{}
		s.EmitTypes = true
		glog.Infof("%s AST with Type Annotation:\n%s", name, s.Dump(ast))
	}

	obj, err := codegen.CodeGen(name, ast)
	if err != nil {
		return nil, err
	}

	vm := New(name, obj, syslogUseCurrentYear, loc)
	return vm, nil
}
