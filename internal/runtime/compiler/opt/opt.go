// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// package opt has a compiler pass for making optimisations on the AST.
package opt

import (
	"github.com/google/mtail/internal/runtime/compiler/ast"
	"github.com/google/mtail/internal/runtime/compiler/parser"
)

func Optimise(n ast.Node) (ast.Node, error) {
	o := &optimiser{}
	return ast.Walk(o, n), nil
}

type optimiser struct {
}

func (o *optimiser) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	return o, node
}

func (o *optimiser) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.BinaryExpr:
		lhs, ok := n.Lhs.(*ast.IntLit)
		if !ok {
			return node
		}
		rhs, ok := n.Rhs.(*ast.IntLit)
		if !ok {
			return node
		}
		var r int64
		switch n.Op {
		case parser.PLUS:
			r = lhs.I + rhs.I
		default:
			return node
		}
		return &ast.IntLit{I: r}
	}
	return node
}
