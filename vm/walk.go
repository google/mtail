// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import "fmt"

// Visitor VisitBefore method is invoked for each node encountered by Walk.
// If the result Visitor v is not nil, Walk visits each of the children of that
// node with v.  VisitAfter is called on n at the end.
type Visitor interface {
	VisitBefore(n astNode) (v Visitor)
	VisitAfter(n astNode)
}

// convenience function
func walknodelist(v Visitor, list []astNode) {
	for _, x := range list {
		Walk(v, x)
	}
}

func Walk(v Visitor, node astNode) {
	// Returning nil from VisitBefore signals to Walk that the Visitor has
	// handled the children of this node.  VisitAfter will not be called.
	if v := v.VisitBefore(node); v == nil {
		return
	}

	switch n := node.(type) {
	case *stmtlistNode:
		walknodelist(v, n.children)
	case *exprlistNode:
		walknodelist(v, n.children)

	case *condNode:
		if n.cond != nil {
			Walk(v, n.cond)
		}
		Walk(v, n.truthNode)
		if n.elseNode != nil {
			Walk(v, n.elseNode)
		}

	case *builtinNode:
		if n.args != nil {
			Walk(v, n.args)
		}

	case *binaryExprNode:
		Walk(v, n.lhs)
		Walk(v, n.rhs)

	case *unaryExprNode:
		Walk(v, n.expr)

	case *indexedExprNode:
		Walk(v, n.index)
		Walk(v, n.lhs)

	case *decoDefNode:
		Walk(v, n.block)

	case *decoNode:
		Walk(v, n.block)

	case *convNode:
		Walk(v, n.n)

	case *patternExprNode:
		Walk(v, n.expr)

	case *patternFragmentDefNode:
		Walk(v, n.expr)

	case *idNode, *caprefNode, *declNode, *stringConstNode, *intConstNode, *floatConstNode, *patternConstNode, *nextNode, *otherwiseNode, *delNode:
		// These nodes are terminals, thus have no children to walk.

	default:
		panic(fmt.Sprintf("Walk: unexpected node type %T: %v", n, n))
	}

	v.VisitAfter(node)
}
