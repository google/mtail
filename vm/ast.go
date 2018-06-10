// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"sync"

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

	typMu sync.RWMutex
	typ   Type
}

func (n *exprlistNode) Pos() *position {
	return mergepositionlist(n.children)
}

func (n *exprlistNode) Type() Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *exprlistNode) SetType(t Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
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

type idNode struct {
	pos    position
	name   string
	sym    *Symbol
	lvalue bool // If set, then this node appears on the left side of an
	// assignment and needs to have its address taken only.
}

func (n *idNode) Pos() *position {
	return &n.pos
}

func (n *idNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return Error // id not defined
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
		return n.sym.Type
	}
	return Error // sym not defined due to undefined capref error
}

type builtinNode struct {
	pos  position
	name string
	args astNode

	typMu sync.RWMutex
	typ   Type
}

func (n *builtinNode) Pos() *position {
	return &n.pos
}

func (n *builtinNode) Type() Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *builtinNode) SetType(t Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type binaryExprNode struct {
	lhs, rhs astNode
	op       int

	typMu sync.RWMutex
	typ   Type
}

func (n *binaryExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.rhs.Pos())
}

func (n *binaryExprNode) Type() Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *binaryExprNode) SetType(t Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type unaryExprNode struct {
	pos  position // pos is the position of the op
	expr astNode
	op   int

	typMu sync.RWMutex
	typ   Type
}

func (n *unaryExprNode) Pos() *position {
	return MergePosition(&n.pos, n.expr.Pos())
}

func (n *unaryExprNode) Type() Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *unaryExprNode) SetType(t Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type indexedExprNode struct {
	lhs, index astNode

	typMu sync.RWMutex
	typ   Type
}

func (n *indexedExprNode) Pos() *position {
	return MergePosition(n.lhs.Pos(), n.index.Pos())
}

func (n *indexedExprNode) Type() Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *indexedExprNode) SetType(t Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
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
		return n.sym.Type
	}
	return Error
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

// patternExprNode is the top of a pattern expression
type patternExprNode struct {
	expr    astNode
	pattern string // if not empty, the fully defined pattern after typecheck
	index   int    // reference to the compiled object offset after codegen
}

func (n *patternExprNode) Pos() *position {
	return n.expr.Pos()
}

func (n *patternExprNode) Type() Type {
	return Pattern
}

// patternConstNode holds inline constant pattern fragments
type patternConstNode struct {
	pos     position
	pattern string
}

func (n *patternConstNode) Pos() *position {
	return &n.pos
}

func (n *patternConstNode) Type() Type {
	return Pattern
}

// patternDefNode holds a named pattern expression
type patternFragmentDefNode struct {
	id      astNode
	expr    astNode
	sym     *Symbol // Optional Symbol for a named pattern
	pattern string  // If not empty, contains the complete evaluated pattern of the expr
}

func (n *patternFragmentDefNode) Pos() *position {
	return n.id.Pos()
}

func (n *patternFragmentDefNode) Type() Type {
	return Pattern
}

type decoDefNode struct {
	pos   position
	name  string
	block astNode
	sym   *Symbol
	scope *Scope
}

func (n *decoDefNode) Pos() *position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *decoDefNode) Type() Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return Int
}

type decoNode struct {
	pos   position
	name  string
	block astNode
	def   *decoDefNode
	scope *Scope
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

type convNode struct {
	n astNode

	mu  sync.RWMutex
	typ Type
}

func (n *convNode) Pos() *position {
	return n.n.Pos()
}

func (n *convNode) Type() Type {
	n.mu.RLock()
	defer n.mu.RUnlock()
	return n.typ
}

func (n *convNode) SetType(t Type) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.typ = t
}

type errorNode struct {
	pos      position
	spelling string
}

func (n *errorNode) Pos() *position {
	return &n.pos
}

func (n *errorNode) Type() Type {
	return Error
}
