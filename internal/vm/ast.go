// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"sync"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/position"
	"github.com/google/mtail/internal/vm/symtab"
	"github.com/google/mtail/internal/vm/types"
)

type astNode interface {
	Pos() *position.Position // Returns the position of the node from the original source
	Type() types.Type        // Returns the type of the expression in this node
}

type StmtList struct {
	Scope    *symtab.Scope // Pointer to the local scope for this enclosing block
	children []astNode
}

func (n *StmtList) Pos() *position.Position {
	return mergepositionlist(n.children)
}

func (n *StmtList) Type() types.Type {
	return types.None
}

type ExprList struct {
	children []astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *ExprList) Pos() *position.Position {
	return mergepositionlist(n.children)
}

func (n *ExprList) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *ExprList) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type Cond struct {
	cond      astNode
	truthNode astNode
	elseNode  astNode
	s         *symtab.Scope // a conditional expression can cause new variables to be defined
}

func (n *Cond) Pos() *position.Position {
	return mergepositionlist([]astNode{n.cond, n.truthNode, n.elseNode})
}

func (n *Cond) Type() types.Type {
	return types.None
}

type Id struct {
	pos    position.Position
	name   string
	sym    *symtab.Symbol
	lvalue bool // If set, then this node appears on the left side of an
	// assignment and needs to have its address taken only.
}

func (n *Id) Pos() *position.Position {
	return &n.pos
}

func (n *Id) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error // id not defined
}

type CaprefNode struct {
	pos     position.Position
	name    string
	isNamed bool // true if the capref is a named reference, not positional
	sym     *symtab.Symbol
}

func (n *CaprefNode) Pos() *position.Position {
	return &n.pos
}

func (n *CaprefNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error // sym not defined due to undefined capref error
}

type BuiltinNode struct {
	pos  position.Position
	name string
	args astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BuiltinNode) Pos() *position.Position {
	return &n.pos
}

func (n *BuiltinNode) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *BuiltinNode) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type BinaryExpr struct {
	lhs, rhs astNode
	op       int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BinaryExpr) Pos() *position.Position {
	return MergePosition(n.lhs.Pos(), n.rhs.Pos())
}

func (n *BinaryExpr) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *BinaryExpr) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type UnaryExpr struct {
	pos  position.Position // pos is the position of the op
	expr astNode
	op   int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *UnaryExpr) Pos() *position.Position {
	return MergePosition(&n.pos, n.expr.Pos())
}

func (n *UnaryExpr) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *UnaryExpr) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type IndexedExpr struct {
	lhs, index astNode

	typMu sync.RWMutex
	typ   types.Type
}

func (n *IndexedExpr) Pos() *position.Position {
	return MergePosition(n.lhs.Pos(), n.index.Pos())
}

func (n *IndexedExpr) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *IndexedExpr) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type DeclNode struct {
	pos          position.Position
	name         string
	hidden       bool
	keys         []string
	kind         metrics.Kind
	exportedName string
	sym          *symtab.Symbol
}

func (n *DeclNode) Pos() *position.Position {
	return &n.pos
}

func (n *DeclNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Error
}

type StringConst struct {
	pos  position.Position
	text string
}

func (n *StringConst) Pos() *position.Position {
	return &n.pos
}
func (n *StringConst) Type() types.Type {
	return types.String
}

type IntConst struct {
	pos position.Position
	i   int64
}

func (n *IntConst) Pos() *position.Position {
	return &n.pos
}
func (n *IntConst) Type() types.Type {
	return types.Int
}

type FloatConst struct {
	pos position.Position
	f   float64
}

func (n *FloatConst) Pos() *position.Position {
	return &n.pos
}
func (n *FloatConst) Type() types.Type {
	return types.Float
}

// patternExprNode is the top of a pattern expression
type PatternExpr struct {
	expr    astNode
	pattern string // if not empty, the fully defined pattern after typecheck
	index   int    // reference to the compiled object offset after codegen
}

func (n *PatternExpr) Pos() *position.Position {
	return n.expr.Pos()
}

func (n *PatternExpr) Type() types.Type {
	return types.Pattern
}

// patternConstNode holds inline constant pattern fragments
type PatternConst struct {
	pos     position.Position
	pattern string
}

func (n *PatternConst) Pos() *position.Position {
	return &n.pos
}

func (n *PatternConst) Type() types.Type {
	return types.Pattern
}

// patternDefNode holds a named pattern expression
type PatternFragmentDefNode struct {
	id      astNode
	expr    astNode
	sym     *symtab.Symbol // Optional Symbol for a named pattern
	pattern string         // If not empty, contains the complete evaluated pattern of the expr
}

func (n *PatternFragmentDefNode) Pos() *position.Position {
	return n.id.Pos()
}

func (n *PatternFragmentDefNode) Type() types.Type {
	return types.Pattern
}

type DecoDefNode struct {
	pos   position.Position
	name  string
	block astNode
	sym   *symtab.Symbol
	scope *symtab.Scope
}

func (n *DecoDefNode) Pos() *position.Position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *DecoDefNode) Type() types.Type {
	if n.sym != nil {
		return n.sym.Type
	}
	return types.Int
}

type DecoNode struct {
	pos   position.Position
	name  string
	block astNode
	def   *DecoDefNode
	scope *symtab.Scope
}

func (n *DecoNode) Pos() *position.Position {
	return MergePosition(&n.pos, n.block.Pos())
}

func (n *DecoNode) Type() types.Type {
	return types.None
}

type NextNode struct {
	pos position.Position
}

func (n *NextNode) Pos() *position.Position {
	return &n.pos
}

func (n *NextNode) Type() types.Type {
	return types.None
}

type OtherwiseNode struct {
	pos position.Position
}

func (n *OtherwiseNode) Pos() *position.Position {
	return &n.pos
}

func (n *OtherwiseNode) Type() types.Type {
	return types.None
}

type DelNode struct {
	pos    position.Position
	n      astNode
	expiry time.Duration
}

func (d *DelNode) Pos() *position.Position {
	return &d.pos
}

func (d *DelNode) Type() types.Type {
	return types.None
}

type ConvNode struct {
	n astNode

	mu  sync.RWMutex
	typ types.Type
}

func (n *ConvNode) Pos() *position.Position {
	return n.n.Pos()
}

func (n *ConvNode) Type() types.Type {
	n.mu.RLock()
	defer n.mu.RUnlock()
	return n.typ
}

func (n *ConvNode) SetType(t types.Type) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.typ = t
}

type ErrorNode struct {
	pos      position.Position
	spelling string
}

func (n *ErrorNode) Pos() *position.Position {
	return &n.pos
}

func (n *ErrorNode) Type() types.Type {
	return types.Error
}

type StopNode struct {
	pos position.Position
}

func (n *StopNode) Pos() *position.Position {
	return &n.pos
}

func (n *StopNode) Type() types.Type {
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
