// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package compiler

import (
	"io"
	"path/filepath"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/runtime/code"
	"github.com/google/mtail/internal/runtime/compiler/checker"
	"github.com/google/mtail/internal/runtime/compiler/codegen"
	"github.com/google/mtail/internal/runtime/compiler/parser"
)

// Compile compiles a program from the input into bytecode and data stored in an Object, or a list
// of compile errors.
func Compile(name string, input io.Reader, emitAst bool, emitAstTypes bool, maxRegexpLength int, maxRecursionDepth int) (*code.Object, error) { // TODO this is a prime candidate for Options pattern. See https://github.com/google/mtail/pull/474#discussion_r598044460
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
	return obj, nil
}
