// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "fmt"

// Visitor VisitBefore method is invoked for each node encountered by Walk.
// If the result Visitor v is not nil, Walk visits each of the children of that
// node with v.  VisitAfter is called on n at the end.
type Visitor interface {
	VisitBefore(n astNode) (Visitor, astNode)
	VisitAfter(n astNode) astNode
}

// convenience function
func walknodelist(v Visitor, list []astNode) []astNode {
	var r []astNode
	for _, x := range list {
		r = append(r, Walk(v, x))
	}
	return r
}

// Walk traverses (walks) an AST node with the provided Visitor v.
func Walk(v Visitor, node astNode) astNode {
	// Returning nil from VisitBefore signals to Walk that the Visitor has
	// handled the children of this node.  VisitAfter will not be called.
	if v, node = v.VisitBefore(node); v == nil {
		return node
	}

	switch n := node.(type) {
	case *StmtList:
		n.children = walknodelist(v, n.children)
	case *ExprList:
		n.children = walknodelist(v, n.children)

	case *Cond:
		if n.cond != nil {
			n.cond = Walk(v, n.cond)
		}
		n.truthNode = Walk(v, n.truthNode)
		if n.elseNode != nil {
			n.elseNode = Walk(v, n.elseNode)
		}

	case *BuiltinNode:
		if n.args != nil {
			n.args = Walk(v, n.args)
		}

	case *BinaryExpr:
		n.lhs = Walk(v, n.lhs)
		n.rhs = Walk(v, n.rhs)

	case *UnaryExpr:
		n.expr = Walk(v, n.expr)

	case *IndexedExpr:
		n.index = Walk(v, n.index)
		n.lhs = Walk(v, n.lhs)

	case *DecoDefNode:
		n.block = Walk(v, n.block)

	case *DecoNode:
		n.block = Walk(v, n.block)

	case *ConvNode:
		n.n = Walk(v, n.n)

	case *PatternExpr:
		n.expr = Walk(v, n.expr)

	case *PatternFragmentDefNode:
		n.expr = Walk(v, n.expr)

	case *Id, *CaprefNode, *DeclNode, *StringConst, *IntConst, *FloatConst, *PatternConst, *NextNode, *OtherwiseNode, *DelNode, *StopNode:
		// These nodes are terminals, thus have no children to walk.

	default:
		panic(fmt.Sprintf("Walk: unexpected node type %T: %v", n, n))
	}

	node = v.VisitAfter(node)
	return node
}
