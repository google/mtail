// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"

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
			n.sym = sym
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Capture group `$%s' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<%s>...)' to name the capture group.", n.name, n.name))
			return nil
		}

	case *declNode:
		glog.Infof("Checking this decl: %q", n.name)

		if sym, ok := c.symtab.Lookup(n.name, IDSymbol); ok {
			glog.Infof("Found this guy: %v", sym)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("No metric for %q found", n.name))
			return nil
		}

	case *defNode:

	case *decoNode:
		if sym, ok := c.symtab.Lookup(n.name, DefSymbol); ok {
			if sym.binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.name))
				return nil
			}
			n.def = sym.binding.(*defNode)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.name, n.name))
			return nil
		}

	case *idNode:
		if sym, ok := c.symtab.Lookup(n.name, IDSymbol); ok {
			n.sym = sym
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\tTry adding `counter %s' to the top of the program.", n.name, n.name))
			return nil
		}

	case *regexNode:
		if re, err := syntax.Parse(n.pattern, syntax.Perl); err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf(err.Error()))
			return nil
		} else {
			n.re_ast = re
			// We can reserve storage for these capturing groups, storing them in
			// the current scope, so that future CAPTUREGROUPs can retrieve their
			// value.  At parse time, we can warn about nonexistent names.
			for i := 1; i <= re.MaxCap(); i++ {
				sym := c.symtab.Add(fmt.Sprintf("%d", i),
					CaprefSymbol, n.Pos())
				sym.binding = n
				sym.addr = i - 1
			}
			for i, capref := range re.CapNames() {
				if capref != "" {
					sym := c.symtab.Add(capref, CaprefSymbol, n.Pos())
					sym.binding = n
					sym.addr = i
				}
			}
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
