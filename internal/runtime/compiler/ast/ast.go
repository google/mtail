// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package ast

import (
	"sync"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/runtime/compiler/position"
	"github.com/google/mtail/internal/runtime/compiler/symbol"
	"github.com/google/mtail/internal/runtime/compiler/types"
)

type Node interface {
	Pos() *position.Position // Returns the position of the node from the original source
	Type() types.Type        // Returns the type of the expression in this node
}

type StmtList struct {
	Scope    *symbol.Scope // Pointer to the local scope for this enclosing block
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

type CondStmt struct {
	Cond  Node
	Truth Node
	Else  Node
	Scope *symbol.Scope // a conditional expression can cause new variables to be defined
}

func (n *CondStmt) Pos() *position.Position {
	return mergepositionlist([]Node{n.Cond, n.Truth, n.Else})
}

func (n *CondStmt) Type() types.Type {
	return types.None
}

type IDTerm struct {
	P      position.Position
	Name   string
	Symbol *symbol.Symbol
	Lvalue bool // If set, then this node appears on the left side of an
	// assignment and needs to have its address taken only.
}

func (n *IDTerm) Pos() *position.Position {
	return &n.P
}

func (n *IDTerm) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error // id not defined
}

type CaprefTerm struct {
	P       position.Position
	Name    string
	IsNamed bool // true if the capref is a named reference, not positional
	Symbol  *symbol.Symbol
}

func (n *CaprefTerm) Pos() *position.Position {
	return &n.P
}

func (n *CaprefTerm) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error // sym not defined due to undefined capref error
}

type BuiltinExpr struct {
	P    position.Position
	Name string
	Args Node

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BuiltinExpr) Pos() *position.Position {
	return &n.P
}

func (n *BuiltinExpr) Type() types.Type {
	n.typMu.RLock()
	defer n.typMu.RUnlock()
	return n.typ
}

func (n *BuiltinExpr) SetType(t types.Type) {
	n.typMu.Lock()
	defer n.typMu.Unlock()
	n.typ = t
}

type BinaryExpr struct {
	LHS, RHS Node
	Op       int

	typMu sync.RWMutex
	typ   types.Type
}

func (n *BinaryExpr) Pos() *position.Position {
	return position.Merge(n.LHS.Pos(), n.RHS.Pos())
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
	return position.Merge(&n.P, n.Expr.Pos())
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
	LHS, Index Node

	typMu sync.RWMutex
	typ   types.Type
}

func (n *IndexedExpr) Pos() *position.Position {
	return position.Merge(n.LHS.Pos(), n.Index.Pos())
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

type VarDecl struct {
	P            position.Position
	Name         string
	Hidden       bool
	Keys         []string
	Limit        int64
	Buckets      []float64
	Kind         metrics.Kind
	ExportedName string
	Symbol       *symbol.Symbol
}

func (n *VarDecl) Pos() *position.Position {
	return &n.P
}

func (n *VarDecl) Type() types.Type {
	if n.Kind == metrics.Histogram {
		return types.Buckets
	} else if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Error
}

type StringLit struct {
	P    position.Position
	Text string
}

func (n *StringLit) Pos() *position.Position {
	return &n.P
}

func (n *StringLit) Type() types.Type {
	return types.String
}

type IntLit struct {
	P position.Position
	I int64
}

func (n *IntLit) Pos() *position.Position {
	return &n.P
}

func (n *IntLit) Type() types.Type {
	return types.Int
}

type FloatLit struct {
	P position.Position
	F float64
}

func (n *FloatLit) Pos() *position.Position {
	return &n.P
}

func (n *FloatLit) Type() types.Type {
	return types.Float
}

// PatternExpr is the top of a pattern expression.
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

// PatternLit holds inline constant pattern fragments.
type PatternLit struct {
	P       position.Position
	Pattern string
}

func (n *PatternLit) Pos() *position.Position {
	return &n.P
}

func (n *PatternLit) Type() types.Type {
	return types.Pattern
}

// PatternFragment holds a named pattern part.
type PatternFragment struct {
	ID      Node
	Expr    Node
	Symbol  *symbol.Symbol // Optional Symbol for a named pattern
	Pattern string         // If not empty, contains the complete evaluated pattern of the expr
}

func (n *PatternFragment) Pos() *position.Position {
	return n.ID.Pos()
}

func (n *PatternFragment) Type() types.Type {
	return types.Pattern
}

type DecoDecl struct {
	P      position.Position
	Name   string
	Block  Node
	Symbol *symbol.Symbol
	Scope  *symbol.Scope // The declaration creates its own scope, as a zygote to be instantiated later.
}

func (n *DecoDecl) Pos() *position.Position {
	return position.Merge(&n.P, n.Block.Pos())
}

func (n *DecoDecl) Type() types.Type {
	if n.Symbol != nil {
		return n.Symbol.Type
	}
	return types.Int
}

type DecoStmt struct {
	P     position.Position
	Name  string
	Block Node
	Decl  *DecoDecl     // Pointer to the declaration of the decorator this statement invokes.
	Scope *symbol.Scope // Instantiated with a copy of the Def's Scope.
}

func (n *DecoStmt) Pos() *position.Position {
	return position.Merge(&n.P, n.Block.Pos())
}

func (n *DecoStmt) Type() types.Type {
	return types.None
}

type NextStmt struct {
	P position.Position
}

func (n *NextStmt) Pos() *position.Position {
	return &n.P
}

func (n *NextStmt) Type() types.Type {
	return types.None
}

type OtherwiseStmt struct {
	P position.Position
}

func (n *OtherwiseStmt) Pos() *position.Position {
	return &n.P
}

func (n *OtherwiseStmt) Type() types.Type {
	return types.None
}

type DelStmt struct {
	P      position.Position
	N      Node
	Expiry time.Duration
}

func (n *DelStmt) Pos() *position.Position {
	return &n.P
}

func (n *DelStmt) Type() types.Type {
	return types.None
}

type ConvExpr struct {
	N Node

	mu  sync.RWMutex
	typ types.Type
}

func (n *ConvExpr) Pos() *position.Position {
	return n.N.Pos()
}

func (n *ConvExpr) Type() types.Type {
	n.mu.RLock()
	defer n.mu.RUnlock()
	return n.typ
}

func (n *ConvExpr) SetType(t types.Type) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.typ = t
}

type Error struct {
	P        position.Position
	Spelling string
}

func (n *Error) Pos() *position.Position {
	return &n.P
}

func (n *Error) Type() types.Type {
	return types.Error
}

type StopStmt struct {
	P position.Position
}

func (n *StopStmt) Pos() *position.Position {
	return &n.P
}

func (n *StopStmt) Type() types.Type {
	return types.None
}

// mergepositionlist is a helper that merges the positions of all the nodes in a list.
func mergepositionlist(l []Node) *position.Position {
	switch len(l) {
	case 0:
		return nil
	case 1:
		if l[0] == nil {
			return nil
		}
		return l[0].Pos()
	default:
		return position.Merge(l[0].Pos(), mergepositionlist(l[1:]))
	}
}
