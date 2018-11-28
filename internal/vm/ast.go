// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"sync"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/position"
	"github.com/google/mtail/internal/vm/types"
)

type astNode interface {
	Pos() *position.Position // Returns the position of the node from the original source
	Type() types.Type        // Returns the type of the expression in this node
}

type stmtlistNode struct {
	s        *Scope // Pointer to the local scope for this enclosing block
	children []astNode
}

func (n *stmtlistNode) Pos() *position.Position {
	return mergepositionlist(n.children)
}

func (n *stmtlistNode) Type() types.Type {
	return types.None
}

type exprlistNode struct {
	children []astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *exprlistNode) Pos() *position.Position {
	return mergepositionlist(n.children)
}

func (n *exprlistNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *exprlistNode) SetType(t types.Type) {
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

func (n *condNode) Pos() *position.Position {
	return mergepositionlist([]astNode{n.cond, n.truthNode, n.elseNode})
}

func (n *condNode) Type() types.Type {
	return types.None
}

type idNode struct {
	pos    position.Position
	name   string
	sym    *Symbol
	lvalue bool // If set, then this node appears on the left side of an
	// assignment and needs to have its address taken only.
}

func (n *idNode) Pos() *position.Position {
	return &n.pos
}

func (n *idNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error // id not defined
}

type caprefNode struct {
	pos     position.Position
	name    string
	isNamed bool // true if the capref is a named reference, not positional
	sym     *Symbol
}

func (n *caprefNode) Pos() *position.Position {
	return &n.pos
}

func (n *caprefNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error // sym not defined due to undefined capref error
}

type builtinNode struct {
	pos  position.Position
	name string
	args astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *builtinNode) Pos() *position.Position {
	return &n.pos
}

func (n *builtinNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *builtinNode) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type binaryExprNode struct {
	lhs, rhs astNode
	op       int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *binaryExprNode) Pos() *position.Position {
	return MergePosition(n.lhs.Pos(), n.rhs.Pos())
}

func (n *binaryExprNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *binaryExprNode) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type unaryExprNode struct {
	pos  position.Position // pos is the position of the op
	expr astNode
	op   int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *unaryExprNode) Pos() *position.Position {
	return MergePosition(&n.pos, n.expr.Pos())
}

func (n *unaryExprNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *unaryExprNode) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type indexedExprNode struct {
	lhs, index astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *indexedExprNode) Pos() *position.Position {
	return MergePosition(n.lhs.Pos(), n.index.Pos())
}

func (n *indexedExprNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *indexedExprNode) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type declNode struct {
	pos          position.Position
	name         string
	hidden       bool
	keys         []string
	kind         metrics.Kind
	exportedName string
	sym          *Symbol
}

func (n *declNode) Pos() *position.Position {
	return &n.pos
}

func (n *declNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error
}

type stringConstNode struct {
	pos  position.Position
	text string
}

func (n *stringConstNode) Pos() *position.Position {
	return &n.pos
}
func (n *stringConstNode) Type() types.Type {
	return types.String
}

type intConstNode struct {
	pos position.Position
	i   int64
}

func (n *intConstNode) Pos() *position.Position {
	return &n.pos
}
func (n *intConstNode) Type() types.Type {
	return types.Int
}

type floatConstNode struct {
	pos position.Position
	f   float64
}

func (n *floatConstNode) Pos() *position.Position {
	return &n.pos
}
func (n *floatConstNode) Type() types.Type {
	return types.Float
}

// patternExprNode is the top of a pattern expression
type patternExprNode struct {
	expr    astNode
	pattern string // if not empty, the fully defined pattern after typecheck
	index   int    // reference to the compiled object offset after codegen
}

func (n *patternExprNode) Pos() *position.Position {
	return n.expr.Pos()
}

func (n *patternExprNode) Type() types.Type {
	return types.Pattern
}

// patternConstNode holds inline constant pattern fragments
type patternConstNode struct {
	pos     position.Position
	pattern string
}

func (n *patternConstNode) Pos() *position.Position {
	return &n.pos
}

func (n *patternConstNode) Type() types.Type {
	return types.Pattern
}

// patternDefNode holds a named pattern expression
type patternFragmentDefNode struct {
	id      astNode
	expr    astNode
	sym     *Symbol // Optional Symbol for a named pattern
	pattern string  // If not empty, contains the complete evaluated pattern of the expr
}

func (n *patternFragmentDefNode) Pos() *position.Position {
	return n.id.Pos()
}

func (n *patternFragmentDefNode) Type() types.Type {
	return types.Pattern
}

type decoDefNode struct {
	pos   position.Position
	name  string
	block astNode
	sym   *Symbol
	scope *Scope
}

func (n *decoDefNode) Pos() *position.Position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *decoDefNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Int
}

type decoNode struct {
	pos   position.Position
	name  string
	block astNode
	def   *decoDefNode
	scope *Scope
}

func (n *decoNode) Pos() *position.Position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *decoNode) Type() types.Type {
	return types.None
}

type nextNode struct {
	pos position.Position
}

func (n *nextNode) Pos() *position.Position {
	return &n.pos
}

func (n *nextNode) Type() types.Type {
	return types.None
}

type otherwiseNode struct {
	pos position.Position
}

func (n *otherwiseNode) Pos() *position.Position {
	return &n.pos
}

func (n *otherwiseNode) Type() types.Type {
	return types.None
}

type delNode struct {
	pos    position.Position
	n      astNode
	expiry time.Duration
}

func (d *delNode) Pos() *position.Position {
	return &d.pos
}

func (d *delNode) Type() types.Type {
	return types.None
}

type convNode struct {
	n astNode

	mu  sync.RWMutex
	typ types.Type
}

func (n *convNode) Pos() *position.Position {
	return n.n.Pos()
}

func (n *convNode) Type() types.Type {
	n.mu.RLock()
	defer n.mu.RUnlock()
	return n.typ
}

func (n *convNode) SetType(t types.Type) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.typ = t
}

type errorNode struct {
	pos      position.Position
	spelling string
}

func (n *errorNode) Pos() *position.Position {
	return &n.pos
}

func (n *errorNode) Type() types.Type {
	return types.Error
}

type stopNode struct {
	pos position.Position
}

func (n *stopNode) Pos() *position.Position {
	return &n.pos
}

func (n *stopNode) Type() types.Type {
	return types.None
}

// MergePosition returns the union of two positions such that the result contains both inputs.
func MergePosition(a, b *position.Position) *position.Position {
	if a == nil {
		return b
	}
	if b == nil {
		return a
	}
	if a.Filename != b.Filename {
		return a
	}
	// TODO(jaq): handle multi-line positions
	if a.Line != b.Line {
		return a
	}
	r := *a
	if b.Startcol < r.Startcol {
		r.Startcol = b.Startcol
	}
	if b.Endcol > r.Endcol {
		r.Endcol = b.Endcol
	}
	return &r
}

// mergepositionlist is a helper that merges the positions of all the nodes in a list
func mergepositionlist(l []astNode) *position.Position {
	if len(l) == 0 {
		return nil
	}
	if len(l) == 1 {
		if l[0] != nil {
			return l[0].Pos()
		}
		return nil
	}
	return MergePosition(l[0].Pos(), mergepositionlist(l[1:]))
}
