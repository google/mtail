// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type node interface {
}

type stmtlistNode struct {
	s        *scope
	children []node
}

type exprlistNode struct {
	children []node
}

type condNode struct {
	cond     node
	children []node
}

type regexNode struct {
	pattern string
}

type stringNode struct {
	text string
}

type idNode struct {
	name string
}

type caprefNode struct {
	name  string
	index int
}

type builtinNode struct {
	name string
	args node
}

type additiveExprNode struct {
	lhs node
	rhs node
	op  int
}

type assignExprNode struct {
	lhs node
	rhs node
}

type indexedExprNode struct {
	lhs   node
	index node
}

type declNode struct {
	name     string
	kind     metric_type
	exported bool
}
