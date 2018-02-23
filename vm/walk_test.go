// Copyright 2018 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "testing"

type testNode struct {
}

func (t testNode) Pos() *position {
	return &position{}
}

func (t testNode) Type() Type {
	return None
}

type testVisitor struct {
}

func (v testVisitor) VisitBefore(n astNode) Visitor {
	return v
}

func (v testVisitor) VisitAfter(n astNode) {
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
