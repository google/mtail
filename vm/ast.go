// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp/syntax"

	"github.com/google/mtail/metrics"
)

type astNode interface {
	Pos() *position // Returns the position of the node from the original source
	Type() Type     // Returns the type of the expression in this node
}

type stmtlistNode struct {
	s        *Scope // Pointer to the local scope for this enclosing block
	children []astNode
}

func (n *stmtlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

func (n *stmtlistNode) Type() Type {
	return None
}

type exprlistNode struct {
	children []astNode
}

func (n *exprlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

func (n *exprlistNode) Type() Type {
	return None
}

type condNode struct {
	cond      astNode
	truthNode astNode
	elseNode  astNode
	s         *Scope // a conditional expression can cause new variables to be defined
}

func (n *condNode) Pos() *position {
	return mergepositionlist([]astNode{n.cond, n.truthNode, n.elseNode})
}

func (n *condNode) Type() Type {
	return None
}

type regexNode struct {
	astNode
	pos     position
	pattern string
	addr    int
	re_ast  *syntax.Regexp
}

func (n *regexNode) Pos() *position {
	return &n.pos
}

func (n *regexNode) Type() Type {
	return None
}

type idNode struct {
	pos  position
	name string
	sym  *Symbol
}

func (n *idNode) Pos() *position {
	return &n.pos
}

func (n *idNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type.Root()
	}
	return None // Bugs
}

type caprefNode struct {
	pos     position
	name    string
	isNamed bool // true if the capref is a named reference, not positional
	sym     *Symbol
}

func (n *caprefNode) Pos() *position {
	return &n.pos
}

func (n *caprefNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type.Root()
	}
	return None // sym not defined due to undefined capref error
}

type builtinNode struct {
	pos  position
	name string
	args astNode
}

func (n *builtinNode) Pos() *position {
	return &n.pos
}

func (n *builtinNode) Type() Type {
	return Int
}

type binaryExprNode struct {
	lhs, rhs astNode
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
	expr astNode
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
	lhs, index astNode
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
	sym          *Symbol
}

func (n *declNode) Pos() *position {
	return &n.pos
}

func (n *declNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type.Root()
	}
	return Undef
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
	block astNode
	sym   *Symbol
}

func (n *defNode) Pos() *position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *defNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type.Root()
	}
	return Int
}

type decoNode struct {
	pos   position
	name  string
	block astNode
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

type delNode struct {
	pos position
	n   astNode
}

func (d *delNode) Pos() *position {
	return &d.pos
}

func (d *delNode) Type() Type {
	return None
}
