// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"regexp"
)

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
	addr    int
	re      *regexp.Regexp
}

type stringNode struct {
	text string
}

type idNode struct {
	name string
	sym  *symbol
}

type caprefNode struct {
	name string
	sym  *symbol
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
	name          string
	keys          []string
	kind          metric_type
	exported      bool
	exported_name string
	m             Metric
	sym           *symbol
}

type incByExprNode struct {
	lhs node
	rhs node
}

type incExprNode struct {
	lhs node
}

type constExprNode struct {
	value int
}
