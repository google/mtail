// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
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
		c.symtab.EnterScope(nil)
		n.s = c.symtab.CurrentScope()

	case *caprefNode:
		if sym, ok := c.symtab.Lookup(n.name, CaprefSymbol); ok {
			n.sym = sym
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Capture group `$%s' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<%s>...)' to name the capture group.", n.name, n.name))
			return nil
		}

	case *declNode:
		if sym, ok := c.symtab.Lookup(n.name, IDSymbol); ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("Declaration of `%s' shadows the previous at %s", n.name, sym.loc))
			return nil
		}
		n.sym = c.symtab.Add(n.name, IDSymbol, &n.pos)

	case *defNode:
		n.sym = c.symtab.Add(n.name, DefSymbol, &n.pos)
		(*n.sym).binding = n

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
	switch n := node.(type) {
	case *stmtlistNode:
		c.symtab.ExitScope()

	case *binaryExprNode:
		var rType Type
		Tl := n.lhs.Type()
		Tr := n.rhs.Type()
		switch n.op {
		// case DIV, MOD, MUL, MINUS, PLUS, POW:
		// 	// Numeric
		// case SHL, SHR:
		// 	//  integer
		// case LT, GT, LE, GE, EQ, NE:
		// 	// comparable
		// case AND, OR, XOR, NOT:
		// case ADD_ASSIGN, ASSIGN:
		default:
			if Tl != Tr {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch between lhs (%s) and rhs (%s) for op %s", Tl, Tr, n.op))
			}
			rType = Tl
		}
		n.typ = rType

	case *unaryExprNode:
		n.typ = n.expr.Type()
	}
}
