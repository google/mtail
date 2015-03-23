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

type relNode struct {
	lhs, rhs node
	op       int
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
	lhs, rhs node
	op       int
}

type assignExprNode struct {
	lhs, rhs node
}

type indexedExprNode struct {
	lhs, index node
}

type declNode struct {
	name          string
	keys          []string
	kind          metric_type
	exported_name string
	m             *Metric
	sym           *symbol
}

type incByExprNode struct {
	lhs, rhs node
}

type incExprNode struct {
	lhs node
}

type numericExprNode struct {
	value int
}

type defNode struct {
	name     string
	children []node
	sym      *symbol
}

type decoNode struct {
	name     string
	children []node
	def      *defNode
}

type nextNode struct {
}
