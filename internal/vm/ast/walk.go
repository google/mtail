// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package ast

import "fmt"

// Visitor VisitBefore method is invoked for each node encountered by Walk.
// If the result Visitor v is not nil, Walk visits each of the children of that
// node with v.  VisitAfter is called on n at the end.
type Visitor interface {
	VisitBefore(n Node) (Visitor, Node)
	VisitAfter(n Node) Node
}

// convenience function
func walknodelist(v Visitor, list []Node) []Node {
	var r []Node
	for _, x := range list {
		r = append(r, Walk(v, x))
	}
	return r
}

// Walk traverses (walks) an AST node with the provided Visitor v.
func Walk(v Visitor, node Node) Node {
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

	case *Cond:
		if n.Cond != nil {
			n.Cond = Walk(v, n.Cond)
		}
		n.Truth = Walk(v, n.Truth)
		if n.Else != nil {
			n.Else = Walk(v, n.Else)
		}

	case *BuiltinNode:
		if n.Args != nil {
			n.Args = Walk(v, n.Args)
		}

	case *BinaryExpr:
		n.Lhs = Walk(v, n.Lhs)
		n.Rhs = Walk(v, n.Rhs)

	case *UnaryExpr:
		n.Expr = Walk(v, n.Expr)

	case *IndexedExpr:
		n.Index = Walk(v, n.Index)
		n.Lhs = Walk(v, n.Lhs)

	case *DecoDefNode:
		n.Block = Walk(v, n.Block)

	case *DecoNode:
		n.Block = Walk(v, n.Block)

	case *ConvNode:
		n.N = Walk(v, n.N)

	case *PatternExpr:
		n.Expr = Walk(v, n.Expr)

	case *PatternFragmentDefNode:
		n.Expr = Walk(v, n.Expr)

	case *Id, *CaprefNode, *DeclNode, *StringConst, *IntConst, *FloatConst, *PatternConst, *NextNode, *OtherwiseNode, *DelNode, *StopNode:
		// These nodes are terminals, thus have no children to walk.

	default:
		panic(fmt.Sprintf("Walk: unexpected node type %T: %v", n, n))
	}

	node = v.VisitAfter(node)
	return node
}
