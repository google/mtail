// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp/syntax"

	"github.com/google/mtail/metrics"
)

type node interface {
	Pos() *position // Returns the position of the node from the original source
}

type stmtlistNode struct {
	s        *scope
	children []node
}

func (n *stmtlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

type exprlistNode struct {
	children []node
}

func (n *exprlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

type condNode struct {
	cond      node
	truthNode node
	elseNode  node
}

func (n *condNode) Pos() *position {
	return mergepositionlist([]node{n.cond, n.truthNode, n.elseNode})
}

type regexNode struct {
	pos     position
	pattern string
	addr    int
	re_ast  *syntax.Regexp
}

func (n *regexNode) Pos() *position {
	return &n.pos
}

type idNode struct {
	pos  position
	name string
	sym  *symbol
}

func (n *idNode) Pos() *position {
	return &n.pos
}

type caprefNode struct {
	pos  position
	name string
	sym  *symbol
}

func (n *caprefNode) Pos() *position {
	return &n.pos
}

type builtinNode struct {
	pos  position
	name string
	args node
}

func (n *builtinNode) Pos() *position {
	return &n.pos
}

type binaryExprNode struct {
	lhs, rhs node
	op       int
}

func (n *binaryExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.rhs.Pos())
}

type unaryExprNode struct {
	pos position // pos is the position of the op
	lhs node
	op  int
}

func (n *unaryExprNode) Pos() *position {
	return MergePosition(&n.pos, n.lhs.Pos())
}

type indexedExprNode struct {
	lhs, index node
}

func (n *indexedExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.index.Pos())
}

type declNode struct {
	pos          position
	name         string
	keys         []string
	kind         metrics.Kind
	exportedName string
	m            *metrics.Metric
	sym          *symbol
}

func (n *declNode) Pos() *position {
	return &n.pos
}

type stringConstNode struct {
	pos  position
	text string
}

func (n *stringConstNode) Pos() *position {
	return &n.pos
}

type intConstNode struct {
	pos position
	i   int64
}

func (n *intConstNode) Pos() *position {
	return &n.pos
}

type floatConstNode struct {
	pos position
	f   float64
}

func (n *floatConstNode) Pos() *position {
	return &n.pos
}

type defNode struct {
	pos      position
	name     string
	children node
	sym      *symbol
}

func (n *defNode) Pos() *position {
	return MergePosition(&n.pos, n.children.Pos())
}

type decoNode struct {
	pos      position
	name     string
	children []node
	def      *defNode
}

func (n *decoNode) Pos() *position {
	return MergePosition(&n.pos, mergepositionlist(n.children))
}

type nextNode struct {
	pos position
}

func (n *nextNode) Pos() *position {
	return &n.pos
}

type otherwiseNode struct {
	pos position
}

func (n *otherwiseNode) Pos() *position {
	return &n.pos
}
