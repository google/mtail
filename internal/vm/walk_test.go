// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"sync"
	"testing"

	go_cmp "github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

type testNode struct {
}

func (t testNode) Pos() *Position {
	return &Position{}
}

func (t testNode) Type() Type {
	return None
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
	case *binaryExprNode:
		if v.op == DIV {
			n = &intConstNode{i: 4}
		}
	}
	return t, n
}

func (t *testWalker) VisitAfter(n astNode) astNode {
	switch v := n.(type) {
	case *binaryExprNode:
		if v.op == MINUS {
			n = &intConstNode{i: 5}
		}
	}
	return n
}

func TestAstReplacement(t *testing.T) {
	var a astNode

	a = &binaryExprNode{lhs: &binaryExprNode{lhs: &intConstNode{i: 0}, rhs: &intConstNode{i: 1}, op: DIV},
		rhs: &binaryExprNode{lhs: &intConstNode{i: 2}, rhs: &intConstNode{i: 3}, op: MINUS},
		op:  PLUS}
	tw := &testWalker{}
	a = Walk(tw, a)
	expected := &binaryExprNode{lhs: &intConstNode{i: 4},
		rhs: &intConstNode{i: 5},
		op:  PLUS}
	diff := go_cmp.Diff(expected, a, cmpopts.IgnoreUnexported(sync.RWMutex{}), go_cmp.AllowUnexported(binaryExprNode{}, intConstNode{}))
	if diff != "" {
		t.Error(diff)
		s := Sexp{}
		t.Log("AST:\n" + s.Dump(a))
	}
}
