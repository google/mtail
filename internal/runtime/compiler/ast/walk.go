// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package ast

import (
	"fmt"

	"github.com/golang/glog"
)

// Visitor VisitBefore method is invoked for each node encountered by Walk.
// If the result Visitor v is not nil, Walk visits each of the children of that
// node with v.  VisitAfter is called on n at the end.
type Visitor interface {
	VisitBefore(n Node) (Visitor, Node)
	VisitAfter(n Node) Node
}

// convenience function.
func walknodelist(v Visitor, list []Node) []Node {
	r := make([]Node, 0, len(list))
	for _, x := range list {
		r = append(r, Walk(v, x))
	}
	return r
}

// Walk traverses (walks) an AST node with the provided Visitor v.
func Walk(v Visitor, node Node) Node {
	glog.V(2).Infof("About to VisitBefore node at %s", node.Pos())
	// Returning nil from VisitBefore signals to Walk that the Visitor has
	// handled the children of this node.  VisitAfter will not be called.
	if v, node = v.VisitBefore(node); v == nil {
		return node
	}

	switch n := node.(type) {
	case *StmtList:
		n.Children = walknodelist(v, n.Children)
	case *ExprList:
		n.Children = walknodelist(v, n.Children)

	case *CondStmt:
		if n.Cond != nil {
			n.Cond = Walk(v, n.Cond)
		}
		n.Truth = Walk(v, n.Truth)
		if n.Else != nil {
			n.Else = Walk(v, n.Else)
		}

	case *BuiltinExpr:
		if n.Args != nil {
			n.Args = Walk(v, n.Args)
		}

	case *BinaryExpr:
		n.LHS = Walk(v, n.LHS)
		n.RHS = Walk(v, n.RHS)

	case *UnaryExpr:
		n.Expr = Walk(v, n.Expr)

	case *IndexedExpr:
		n.Index = Walk(v, n.Index)
		n.LHS = Walk(v, n.LHS)

	case *DecoDecl:
		n.Block = Walk(v, n.Block)

	case *DecoStmt:
		n.Block = Walk(v, n.Block)

	case *ConvExpr:
		n.N = Walk(v, n.N)

	case *PatternExpr:
		n.Expr = Walk(v, n.Expr)

	case *PatternFragment:
		n.Expr = Walk(v, n.Expr)

	case *IDTerm, *CaprefTerm, *VarDecl, *StringLit, *IntLit, *FloatLit, *PatternLit, *NextStmt, *OtherwiseStmt, *DelStmt, *StopStmt:
		// These nodes are terminals, thus have no children to walk.

	default:
		panic(fmt.Sprintf("Walk: unexpected node type %T: %v", n, n))
	}

	glog.V(2).Infof("About to VisitAfter node at %s", node.Pos())
	node = v.VisitAfter(node)
	return node
}
