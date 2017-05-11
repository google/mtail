// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
)

// checker holds data for a semantic checker
type checker struct {
	scope *Scope // the current scope

	errors ErrorList
}

// Check performs a semantic check of the AST rooted at node, and returns a boolean
// indicating OK; if ok is not true, then error is a list of errors found.
func Check(node astNode) error {
	c := &checker{}
	Walk(c, node)
	if len(c.errors) > 0 {
		return c.errors
	}
	return nil
}

func (c *checker) VisitBefore(node astNode) Visitor {
	switch n := node.(type) {

	case *stmtlistNode:
		n.s = NewScope(c.scope)
		c.scope = n.s

	case *caprefNode:
		if sym := c.scope.Lookup(n.name); sym == nil || sym.Kind != CaprefSymbol {
			c.errors.Add(n.Pos(), fmt.Sprintf("Capture group `$%s' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<%s>...)' to name the capture group.", n.name, n.name))
			return nil
		} else {
			n.sym = sym
		}

	case *declNode:
		if sym := c.scope.Lookup(n.name); sym != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Declaration of `%s' shadows the previous at %s", n.name, sym.Pos))
			return nil
		}
		n.sym = NewSymbol(n.name, VarSymbol, &n.pos)
		if c.scope.Insert(n.sym) != nil {
			c.errors.Add(&n.pos, fmt.Sprintf("%s already defined", n.sym.Name))
		}

	case *defNode:
		if sym := c.scope.Lookup(n.name); sym != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Definition of decorator `%s' shadows the previous at %s", n.name, sym.Pos))
			return nil
		}
		n.sym = NewSymbol(n.name, DefSymbol, &n.pos)
		(*n.sym).Binding = n
		if c.scope.Insert(n.sym) != nil {
			c.errors.Add(&n.pos, fmt.Sprintf("%s already defined", n.sym.Name))
		}

	case *decoNode:
		if sym := c.scope.Lookup(n.name); sym != nil {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.name))
				return nil
			}
			n.def = sym.Binding.(*defNode)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.name, n.name))
			return nil
		}

	case *idNode:
		if sym := c.scope.Lookup(n.name); sym != nil {
			n.sym = sym
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\tTry adding `counter %s' to the top of the program.", n.name, n.name))
			return nil
		}

	case *delNode:
		Walk(c, n.n)

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
				sym := NewSymbol(fmt.Sprintf("%d", i), CaprefSymbol, n.Pos())
				sym.Binding = n
				sym.Addr = i - 1
				if c.scope.Insert(sym) != nil {
					c.errors.Add(n.Pos(), fmt.Sprintf("%s already defined", sym.Name))
				}
			}
			for i, capref := range re.CapNames() {
				if capref != "" {
					sym := NewSymbol(capref, CaprefSymbol, n.Pos())
					sym.Binding = n
					sym.Addr = i
					if c.scope.Insert(sym) != nil {
						c.errors.Add(n.Pos(), fmt.Sprintf("%s already defined", sym.Name))
					}
				}
			}
		}
	}
	return c
}

func (c *checker) VisitAfter(node astNode) {
	switch n := node.(type) {
	case *stmtlistNode:
		c.scope = n.s.Parent

	case *binaryExprNode:
		var rType Type
		Tl := n.lhs.Type()
		Tr := n.rhs.Type()
		switch n.op {
		//case DIV, MOD, MUL, MINUS, PLUS, POW:
		// Numeric
		// O ⊢ e1 : Tl, O ⊢ e2 : Tr
		// Tl <= Tr , Tr <= Tl
		// ⇒ O ⊢ e : lub(Tl, Tr)
		// case SHL, SHR, AND, OR, XOR, NOT:
		// 	//  integer
		// O ⊢ e1 :Int, O ⊢ e2 : Int
		// ⇒ O ⊢ e : Int
		// case LT, GT, LE, GE, EQ, NE:
		// 	// comparable
		// O ⊢ e1 : Tl, O ⊢ e2 : Tr
		// Tl <= Tr , Tr <= Tl
		// ⇒ O ⊢ e : lub(Tl, Tr)
		// case ASSIGN:
		// O ⊢ e1 : Tl, O ⊢ e2 : Tr
		// Tl <= Tr
		// ⇒ O ⊢ e : Tl
		default:
			if Tl != Tr {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch between lhs (%v) and rhs (%v) for op %d", Tl, Tr, n.op))
			}
			rType = Tl
		}
		n.typ = rType

	case *unaryExprNode:
		switch n.op {
		case NOT:
			n.typ = Int
		default:
			n.typ = n.expr.Type()
		}

	}
}
