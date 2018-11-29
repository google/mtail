// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"sync"
	"testing"

	go_cmp "github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/google/mtail/internal/vm/position"
	"github.com/google/mtail/internal/vm/types"
)

type testNode struct {
}

func (t testNode) Pos() *position.Position {
	return &position.Position{}
}

func (t testNode) Type() types.Type {
	return types.None
}

type testVisitor struct {
}

func (v testVisitor) VisitBefore(n astNode) (Visitor, astNode) {
	return v, n
}

func (v testVisitor) VisitAfter(n astNode) astNode {
	return n
}

func TestWalkPanicsOnUnknown(t *testing.T) {
	defer func() {
		s := recover()
		if s == nil {
			t.Errorf("No panic received")
		}
	}()
	Walk(testVisitor{}, testNode{})
}

type testWalker struct {
}

func (t *testWalker) VisitBefore(n astNode) (Visitor, astNode) {
	switch v := n.(type) {
	case *BinaryExpr:
		if v.op == DIV {
			n = &IntConst{i: 4}
		}
	}
	return t, n
}

func (t *testWalker) VisitAfter(n astNode) astNode {
	switch v := n.(type) {
	case *BinaryExpr:
		if v.op == MINUS {
			n = &IntConst{i: 5}
		}
	}
	return n
}

func TestAstReplacement(t *testing.T) {
	var a astNode

	a = &BinaryExpr{lhs: &BinaryExpr{lhs: &IntConst{i: 0}, rhs: &IntConst{i: 1}, op: DIV},
		rhs: &BinaryExpr{lhs: &IntConst{i: 2}, rhs: &IntConst{i: 3}, op: MINUS},
		op:  PLUS}
	tw := &testWalker{}
	a = Walk(tw, a)
	expected := &BinaryExpr{lhs: &IntConst{i: 4},
		rhs: &IntConst{i: 5},
		op:  PLUS}
	diff := go_cmp.Diff(expected, a, cmpopts.IgnoreUnexported(sync.RWMutex{}), go_cmp.AllowUnexported(BinaryExpr{}, IntConst{}))
	if diff != "" {
		t.Error(diff)
		s := Sexp{}
		t.Log("AST:\n" + s.Dump(a))
	}
}
