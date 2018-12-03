// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package ast

import (
	"sync"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/position"
	"github.com/google/mtail/internal/vm/symtab"
	"github.com/google/mtail/internal/vm/types"
)

type Node interface {
	Pos() *position.Position // Returns the position of the node from the original source
	Type() types.Type        // Returns the type of the expression in this node
}

type StmtList struct {
	Scope    *symtab.Scope // Pointer to the local scope for this enclosing block
	Children []Node
}

func (n *StmtList) Pos() *position.Position {
	return mergepositionlist(n.Children)
}

func (n *StmtList) Type() types.Type {
	return types.None
}

type ExprList struct {
	Children []Node

	typMu sync.RWMutex
	typ   types.Type
}

func (n *ExprList) Pos() *position.Position {
	return mergepositionlist(n.Children)
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
	Cond  Node
	Truth Node
	Else  Node
	Scope *symtab.Scope // a conditional expression can cause new variables to be defined
}

func (n *Cond) Pos() *position.Position {
	return mergepositionlist([]Node{n.Cond, n.Truth, n.Else})
}

func (n *Cond) Type() types.Type {
	return types.None
}

type Id struct {
	P      position.Position
	Name   string
	Symbol *symtab.Symbol
	Lvalue bool // If set, then this node appears on the left side of an
	// assignment and needs to have its address taken only.
}

func (n *Id) Pos() *position.Position {
	return &n.P
}

func (n *Id) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error // id not defined
}

type CaprefNode struct {
	P       position.Position
	Name    string
	IsNamed bool // true if the capref is a named reference, not positional
	Symbol  *symtab.Symbol
}

func (n *CaprefNode) Pos() *position.Position {
	return &n.P
}

func (n *CaprefNode) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error // sym not defined due to undefined capref error
}

type BuiltinNode struct {
	P    position.Position
	Name string
	Args Node

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BuiltinNode) Pos() *position.Position {
	return &n.P
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
	Lhs, Rhs Node
	Op       int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BinaryExpr) Pos() *position.Position {
	return MergePosition(n.Lhs.Pos(), n.Rhs.Pos())
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
	P    position.Position // pos is the position of the op
	Expr Node
	Op   int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *UnaryExpr) Pos() *position.Position {
	return MergePosition(&n.P, n.Expr.Pos())
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
	Lhs, Index Node

	typMu sync.RWMutex
	typ   types.Type
}

func (n *IndexedExpr) Pos() *position.Position {
	return MergePosition(n.Lhs.Pos(), n.Index.Pos())
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
	P            position.Position
	Name         string
	Hidden       bool
	Keys         []string
	Kind         metrics.Kind
	ExportedName string
	Symbol       *symtab.Symbol
}

func (n *DeclNode) Pos() *position.Position {
	return &n.P
}

func (n *DeclNode) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error
}

type StringConst struct {
	P    position.Position
	Text string
}

func (n *StringConst) Pos() *position.Position {
	return &n.P
}
func (n *StringConst) Type() types.Type {
	return types.String
}

type IntConst struct {
	P position.Position
	I int64
}

func (n *IntConst) Pos() *position.Position {
	return &n.P
}
func (n *IntConst) Type() types.Type {
	return types.Int
}

type FloatConst struct {
	P position.Position
	F float64
}

func (n *FloatConst) Pos() *position.Position {
	return &n.P
}
func (n *FloatConst) Type() types.Type {
	return types.Float
}

// patternExprNode is the top of a pattern expression
type PatternExpr struct {
	Expr    Node
	Pattern string // if not empty, the fully defined pattern after typecheck
	Index   int    // reference to the compiled object offset after codegen
}

func (n *PatternExpr) Pos() *position.Position {
	return n.Expr.Pos()
}

func (n *PatternExpr) Type() types.Type {
	return types.Pattern
}

// patternConstNode holds inline constant pattern fragments
type PatternConst struct {
	P       position.Position
	Pattern string
}

func (n *PatternConst) Pos() *position.Position {
	return &n.P
}

func (n *PatternConst) Type() types.Type {
	return types.Pattern
}

// patternDefNode holds a named pattern expression
type PatternFragmentDefNode struct {
	Id      Node
	Expr    Node
	Symbol  *symtab.Symbol // Optional Symbol for a named pattern
	Pattern string         // If not empty, contains the complete evaluated pattern of the expr
}

func (n *PatternFragmentDefNode) Pos() *position.Position {
	return n.Id.Pos()
}

func (n *PatternFragmentDefNode) Type() types.Type {
	return types.Pattern
}

type DecoDefNode struct {
	P      position.Position
	Name   string
	Block  Node
	Symbol *symtab.Symbol
	Scope  *symtab.Scope
}

func (n *DecoDefNode) Pos() *position.Position {
	return MergePosition(&n.P, n.Block.Pos())
}

func (n *DecoDefNode) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Int
}

type DecoNode struct {
	P     position.Position
	Name  string
	Block Node
	Def   *DecoDefNode
	Scope *symtab.Scope
}

func (n *DecoNode) Pos() *position.Position {
	return MergePosition(&n.P, n.Block.Pos())
}

func (n *DecoNode) Type() types.Type {
	return types.None
}

type NextNode struct {
	P position.Position
}

func (n *NextNode) Pos() *position.Position {
	return &n.P
}

func (n *NextNode) Type() types.Type {
	return types.None
}

type OtherwiseNode struct {
	P position.Position
}

func (n *OtherwiseNode) Pos() *position.Position {
	return &n.P
}

func (n *OtherwiseNode) Type() types.Type {
	return types.None
}

type DelNode struct {
	P      position.Position
	N      Node
	Expiry time.Duration
}

func (n *DelNode) Pos() *position.Position {
	return &n.P
}

func (n *DelNode) Type() types.Type {
	return types.None
}

type ConvNode struct {
	N Node

	mu  sync.RWMutex
	typ types.Type
}

func (n *ConvNode) Pos() *position.Position {
	return n.N.Pos()
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
	P        position.Position
	Spelling string
}

func (n *ErrorNode) Pos() *position.Position {
	return &n.P
}

func (n *ErrorNode) Type() types.Type {
	return types.Error
}

type StopNode struct {
	P position.Position
}

func (n *StopNode) Pos() *position.Position {
	return &n.P
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
func mergepositionlist(l []Node) *position.Position {
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
