// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp/syntax"

	"github.com/google/mtail/metrics"
)

type node interface {
	Pos() *position // Returns the position of the node from the original source
	Type() Type     // Returns the type of the expression in this node
}

type stmtlistNode struct {
	s        *scope
	children []node
}

func (n *stmtlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

func (n *stmtlistNode) Type() Type {
	return String
}

type exprlistNode struct {
	children []node
}

func (n *exprlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

func (n *exprlistNode) Type() Type {
	return String
}

type condNode struct {
	cond      node
	truthNode node
	elseNode  node
}

func (n *condNode) Pos() *position {
	return mergepositionlist([]node{n.cond, n.truthNode, n.elseNode})
}

func (n *condNode) Type() Type {
	return None
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

func (n *regexNode) Type() Type {
	return String
}

type idNode struct {
	pos  position
	name string
	sym  *symbol
}

func (n *idNode) Pos() *position {
	return &n.pos
}

func (n *idNode) Type() Type {
	if n.sym != nil {
		return n.sym.typ
	}
	return Int
}

type caprefNode struct {
	pos  position
	name string
	sym  *symbol
}

func (n *caprefNode) Pos() *position {
	return &n.pos
}

func (n *caprefNode) Type() Type {
	if n.sym != nil {
		return n.sym.typ
	}
	return Int
}

type builtinNode struct {
	pos  position
	name string
	args node
}

func (n *builtinNode) Pos() *position {
	return &n.pos
}

func (n *builtinNode) Type() Type {
	return Int
}

type binaryExprNode struct {
	lhs, rhs node
	op       int
	typ      Type
}

func (n *binaryExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.rhs.Pos())
}

func (n *binaryExprNode) Type() Type {
	return n.typ
}

type unaryExprNode struct {
	pos  position // pos is the position of the op
	expr node
	op   int
	typ  Type
}

func (n *unaryExprNode) Pos() *position {
	return MergePosition(&n.pos, n.expr.Pos())
}

func (n *unaryExprNode) Type() Type {
	return n.typ
}

type indexedExprNode struct {
	lhs, index node
}

func (n *indexedExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.index.Pos())
}

func (n *indexedExprNode) Type() Type {
	return n.lhs.Type()
}

type declNode struct {
	pos          position
	name         string
	hidden       bool
	keys         []string
	kind         metrics.Kind
	exportedName string
	sym          *symbol
}

func (n *declNode) Pos() *position {
	return &n.pos
}

func (n *declNode) Type() Type {
	if n.sym != nil {
		return n.sym.typ
	}
	return Int
}

type stringConstNode struct {
	pos  position
	text string
}

func (n *stringConstNode) Pos() *position {
	return &n.pos
}
func (n *stringConstNode) Type() Type {
	return String
}

type intConstNode struct {
	pos position
	i   int64
}

func (n *intConstNode) Pos() *position {
	return &n.pos
}
func (n *intConstNode) Type() Type {
	return Int
}

type floatConstNode struct {
	pos position
	f   float64
}

func (n *floatConstNode) Pos() *position {
	return &n.pos
}
func (n *floatConstNode) Type() Type {
	return Float
}

type defNode struct {
	pos   position
	name  string
	block node
	sym   *symbol
}

func (n *defNode) Pos() *position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *defNode) Type() Type {
	if n.sym != nil {
		return n.sym.typ
	}
	return Int
}

type decoNode struct {
	pos   position
	name  string
	block node
	def   *defNode
}

func (n *decoNode) Pos() *position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *decoNode) Type() Type {
	return None
}

type nextNode struct {
	pos position
}

func (n *nextNode) Pos() *position {
	return &n.pos
}

func (n *nextNode) Type() Type {
	return None
}

type otherwiseNode struct {
	pos position
}

func (n *otherwiseNode) Pos() *position {
	return &n.pos
}

func (n *otherwiseNode) Type() Type {
	return None
}
