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
	case *stmtlistNode:
		n.children = walknodelist(v, n.children)
	case *exprlistNode:
		n.children = walknodelist(v, n.children)

	case *condNode:
		if n.cond != nil {
			n.cond = Walk(v, n.cond)
		}
		n.truthNode = Walk(v, n.truthNode)
		if n.elseNode != nil {
			n.elseNode = Walk(v, n.elseNode)
		}

	case *builtinNode:
		if n.args != nil {
			n.args = Walk(v, n.args)
		}

	case *binaryExprNode:
		n.lhs = Walk(v, n.lhs)
		n.rhs = Walk(v, n.rhs)

	case *unaryExprNode:
		n.expr = Walk(v, n.expr)

	case *indexedExprNode:
		n.index = Walk(v, n.index)
		n.lhs = Walk(v, n.lhs)

	case *decoDefNode:
		n.block = Walk(v, n.block)

	case *decoNode:
		n.block = Walk(v, n.block)

	case *convNode:
		n.n = Walk(v, n.n)

	case *patternExprNode:
		n.expr = Walk(v, n.expr)

	case *patternFragmentDefNode:
		n.expr = Walk(v, n.expr)

	case *idNode, *caprefNode, *declNode, *stringConstNode, *intConstNode, *floatConstNode, *patternConstNode, *nextNode, *otherwiseNode, *delNode, *stopNode:
		// These nodes are terminals, thus have no children to walk.

	default:
		panic(fmt.Sprintf("Walk: unexpected node type %T: %v", n, n))
	}

	node = v.VisitAfter(node)
	return node
}
