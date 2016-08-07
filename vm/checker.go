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

	// symtab contains the current scope search path
	symtab SymbolTable
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
		c.symtab.EnterScope(n.s)
	case *caprefNode:
		if sym, ok := c.symtab.Lookup(n.name, CaprefSymbol); ok {
			glog.Info("Found sym %v", sym)
			n.sym = sym
		} else {
			c.errors.Add(position{}, fmt.Sprintf("Capture group `$%s' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<%s>...)' to name the capture group.", n.name, n.name))
			return nil
		}
	}
	return c
}

func (c *checker) VisitAfter(node node) {
	switch node.(type) {
	case *stmtlistNode:
		c.symtab.ExitScope()
	}
}
