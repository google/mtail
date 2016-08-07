// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"

	"github.com/golang/glog"
)

// checker holds data for a semantic checker
type checker struct {
	errors ErrorList

	// scopes is a stack of enclosing scopes for looking up symbols
	scopes []*scope
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

func (c *checker) currentScope() *scope {
	return c.scopes[len(c.scopes)-1]
}

func (c *checker) VisitBefore(node node) Visitor {
	switch n := node.(type) {
	case *stmtlistNode:
		glog.Info("Adding current scope")
		c.scopes = append(c.scopes, n.s)
	case *caprefNode:
		if sym, ok := c.currentScope().LookupSym(n.name, CaprefSymbol); ok {
			glog.Info("Found sym %v", sym)
			n.sym = sym
		} else {
			c.errors.Add(position{}, fmt.Sprintf("Capture group `$%s' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<%s>...)' to name the capture group.", n.name, n.name))
		}
	}
	return c
}

func (c *checker) VisitAfter(node node) {
	switch node.(type) {
	case *stmtlistNode:
		// Reset the enclosing scope.
		if len(c.scopes) > 1 {
			glog.Info("Resetting enclosing scope")
			c.scopes = c.scopes[:len(c.scopes)-1]
		}
	}
}
