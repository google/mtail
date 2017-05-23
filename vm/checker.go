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

// Check performs a semantic check of the ast node, and returns a list of
// errors found, or nil if the program is semantically valid.  At the
// completion of Check, the symbol table and type annotation is also complete.
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

	case *condNode:
		n.s = NewScope(c.scope)
		c.scope = n.s

	case *caprefNode:
		if sym := c.scope.Lookup(n.name); sym == nil || sym.Kind != CaprefSymbol {
			msg := fmt.Sprintf("Capture group `$%s' was not defined by a regular expression visible to this scope.", n.name)
			if n.isNamed {
				msg = fmt.Sprintf("%s\n\tTry using `(?P<%s>...)' to name the capture group.", msg, n.name)
			} else {
				msg = fmt.Sprintf("%s\n\tCheck that there are at least %s pairs of parentheses.", msg, n.name)
			}
			c.errors.Add(n.Pos(), msg)
			return nil
		} else {
			n.sym = sym
		}

	case *declNode:
		n.sym = NewSymbol(n.name, VarSymbol, &n.pos)
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.name, alt.Pos))
			return nil
		}

	case *idNode:
		if sym := c.scope.Lookup(n.name); sym != nil && sym.Kind == VarSymbol {
			n.sym = sym
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\tTry adding `counter %s' to the top of the program.", n.name, n.name))
			return nil
		}

	case *defNode:
		n.sym = NewSymbol(n.name, DecoSymbol, &n.pos)
		(*n.sym).Binding = n
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of decorator `%s' previously declared at %s", n.name, alt.Pos))
			return nil
		}

	case *decoNode:
		if sym := c.scope.Lookup(n.name); sym != nil && sym.Kind == DecoSymbol {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.name))
				return nil
			}
			n.def = sym.Binding.(*defNode)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.name, n.name))
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
			// We can reserve the names of the capturing groups as declarations
			// of those symbols, so that future CAPREF tokens parsed can
			// retrieve their value.  By recording them in the symbol table, we
			// can warn the user about unknown capture group references.
			for i := 1; i <= re.MaxCap(); i++ {
				sym := NewSymbol(fmt.Sprintf("%d", i), CaprefSymbol, n.Pos())
				sym.Binding = n
				sym.Addr = i - 1
				if alt := c.scope.Insert(sym); alt != nil {
					c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of capture group `%s' previously declared at %s", sym.Name, alt.Pos))
					return nil
				}
			}
			for i, capref := range re.CapNames() {
				if capref != "" {
					sym := NewSymbol(capref, CaprefSymbol, n.Pos())
					sym.Binding = n
					sym.Addr = i
					if alt := c.scope.Insert(sym); alt != nil {
						c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of capture group `%s' previously declared at %s", sym.Name, alt.Pos))
						return nil
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

	case *condNode:
		c.scope = n.s.Parent

	case *binaryExprNode:
		var rType Type
		Tl := n.lhs.Type()
		Tr := n.rhs.Type()
		switch n.op {
		case DIV, MOD, MUL, MINUS, PLUS, POW:
			// Numeric
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = Unify(Tl, Tr)
		case SHL, SHR, AND, OR, XOR, NOT:
			// bitwise
			// O ⊢ e1 :Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			if Equals(Tl, Int) && Equals(Tr, Int) {

				rType = Int
			} else {
				c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise op %q, got %s and %s", n.op, Tl, Tr))
				return
			}
		case LT, GT, LE, GE, EQ, NE:
			// comparable
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = Unify(Tl, Tr)
		case ASSIGN:
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr
			// ⇒ O ⊢ e : Tl
			rType = Tl
		default:
			if Tl != Tr {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch between lhs (%v) and rhs (%v) for op %d", Tl, Tr, n.op))
				return
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
