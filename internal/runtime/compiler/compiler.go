// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package compiler

import (
	"io"
	"path/filepath"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/runtime/code"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/checker"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/codegen"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/opt"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
)

type Compiler struct {
	emitAst             bool
	emitAstTypes        bool
	maxRegexpLength     int
	maxRecursionDepth   int
	disableOptimisation bool
}

func New(options ...Option) (*Compiler, error) {
	c := &Compiler{}
	if err := c.SetOption(options...); err != nil {
		return nil, err
	}
	return c, nil
}

func (c *Compiler) SetOption(options ...Option) error {
	for _, option := range options {
		if err := option(c); err != nil {
			return err
		}
	}
	return nil
}

// Option configures a new Compiler.
type Option func(*Compiler) error

// EmitAst emits the AST after the parse phase.
func EmitAst() Option {
	return func(c *Compiler) error {
		c.emitAst = true
		return nil
	}
}

// EmitAstTypes emits the AST with types after the type checking phase.
func EmitAstTypes() Option {
	return func(c *Compiler) error {
		c.emitAstTypes = true
		return nil
	}
}

// MaxRegexpLength sets the maximum allowable length of a regular expression.
func MaxRegexpLength(maxRegexpLength int) Option {
	return func(c *Compiler) error {
		c.maxRegexpLength = maxRegexpLength
		return nil
	}
}

// MaxRecursionDepth sets the maximum allowable depth of the AST.
func MaxRecursionDepth(maxRecursionDepth int) Option {
	return func(c *Compiler) error {
		c.maxRecursionDepth = maxRecursionDepth
		return nil
	}
}

// DisableOptimisation disables the optimisation phase.
func DisableOptimisation() Option {
	return func(c *Compiler) error {
		c.disableOptimisation = true
		return nil
	}
}

// Compile compiles a program from the input into bytecode and data stored in an Object, or a list
// of compile errors.
func (c *Compiler) Compile(name string, input io.Reader) (obj *code.Object, err error) {
	name = filepath.Base(name)

	var ast ast.Node

	ast, err = parser.Parse(name, input)
	if err != nil {
		return
	}
	if c.emitAst {
		s := parser.Sexp{}
		glog.Infof("%s AST:\n%s", name, s.Dump(ast))
	}

	if !c.disableOptimisation {
		ast, err = opt.Optimise(ast)
		if err != nil {
			return
		}
		if c.emitAstTypes {
			s := parser.Sexp{}
			glog.Infof("Post optimisation %s AST:\n%s", name, s.Dump(ast))
		}
	}

	ast, err = checker.Check(ast, c.maxRegexpLength, c.maxRecursionDepth)
	if err != nil {
		return
	}
	if c.emitAstTypes {
		s := parser.Sexp{}
		s.EmitTypes = true
		glog.Infof("%s AST with Type Annotation:\n%s", name, s.Dump(ast))
	}

	if !c.disableOptimisation {
		ast, err = opt.Optimise(ast)
		if err != nil {
			return
		}
		if c.emitAstTypes {
			s := parser.Sexp{}
			s.EmitTypes = true
			glog.Infof("Post optimisation %s AST with Type Annotation:\n%s", name, s.Dump(ast))
		}
	}

	obj, err = codegen.CodeGen(name, ast)
	return
}
