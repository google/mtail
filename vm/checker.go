// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

// checker holds data for a semantic checker
type checker struct {
	errors ErrorList

	// The enclosing scope for looking up symbols
	currentScope *scope
}

// Check performs a semantic check of the ast node, and returns a boolean
// indicating OK; if ok is not true, then error is a list of errors found.
func Check(node node) error {
	c := &checker{}
	Walk(c, node)
	if len(c.errors) > 0 {
		return c.errors
	}
	return nil
}

func (c *checker) VisitBefore(node node) Visitor {
	switch n := node.(type) {
	case *stmtlistNode:
		c.currentScope = n.s
	}
	return c
}

func (c *checker) VisitAfter(node node) {
	switch n := node.(type) {
	case *stmtlistNode:
		// Reset the enclosing scope.
		c.currentScope = n.s
	}
}
