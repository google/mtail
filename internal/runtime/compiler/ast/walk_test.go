// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package ast_test

import (
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/types"
	"github.com/jaqx0r/mtail/internal/testutil"
)

type testNode struct{}

func (t testNode) Pos() *position.Position {
	return &position.Position{}
}

func (t testNode) Type() types.Type {
	return types.None
}

type testVisitor struct{}

func (v testVisitor) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	return v, n
}

func (v testVisitor) VisitAfter(n ast.Node) ast.Node {
	return n
}

func TestWalkPanicsOnUnknown(t *testing.T) {
	defer func() {
		s := recover()
		if s == nil {
			t.Errorf("No panic received")
		}
	}()
	ast.Walk(testVisitor{}, testNode{})
}

type testWalker struct{}

func (t *testWalker) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	if v, ok := n.(*ast.BinaryExpr); ok {
		if v.Op == parser.DIV {
			n = &ast.IntLit{I: 4}
		}
	}
	return t, n
}

func (t *testWalker) VisitAfter(n ast.Node) ast.Node {
	if v, ok := n.(*ast.BinaryExpr); ok {
		if v.Op == parser.MINUS {
			n = &ast.IntLit{I: 5}
		}
	}
	return n
}

func TestAstReplacement(t *testing.T) {
	var a ast.Node = &ast.BinaryExpr{
		LHS: &ast.BinaryExpr{LHS: &ast.IntLit{I: 0}, RHS: &ast.IntLit{I: 1}, Op: parser.DIV},
		RHS: &ast.BinaryExpr{LHS: &ast.IntLit{I: 2}, RHS: &ast.IntLit{I: 3}, Op: parser.MINUS},
		Op:  parser.PLUS,
	}
	tw := &testWalker{}
	a = ast.Walk(tw, a)
	expected := &ast.BinaryExpr{
		LHS: &ast.IntLit{I: 4},
		RHS: &ast.IntLit{I: 5},
		Op:  parser.PLUS,
	}
	if !testutil.ExpectNoDiff(t, expected, a, testutil.IgnoreUnexported(ast.BinaryExpr{})) {
		s := parser.Sexp{}
		t.Log("AST:\n" + s.Dump(a))
	}
}
