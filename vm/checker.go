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
		if n.sym == nil {
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
		}

	case *declNode:
		n.sym = NewSymbol(n.name, VarSymbol, &n.pos)
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.name, alt.Pos))
			return nil
		}
		// One type per key and one for the value.
		keyTypes := make([]Type, 0, len(n.keys)+1)
		for i := 0; i <= len(n.keys); i++ {
			keyTypes = append(keyTypes, NewTypeVariable())
		}
		n.sym.Type = Function(keyTypes...)

	case *idNode:
		if n.sym == nil {
			glog.Infof("name: %s", n.name)
			if sym := c.scope.Lookup(n.name); sym != nil && sym.Kind == VarSymbol {
				glog.Infof("found sym %v", sym)
				n.sym = sym
			} else {
				c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\tTry adding `counter %s' to the top of the program.", n.name, n.name))
				return nil
			}
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
			// We reserve the names of the capturing groups as declarations
			// of those symbols, so that future CAPREF tokens parsed can
			// retrieve their value.  By recording them in the symbol table, we
			// can warn the user about unknown capture group references.
			for i := 1; i <= re.MaxCap(); i++ {
				sym := NewSymbol(fmt.Sprintf("%d", i), CaprefSymbol, n.Pos())
				sym.Type = inferCaprefType(re, i)
				sym.Binding = n
				sym.Addr = i
				if alt := c.scope.Insert(sym); alt != nil {
					c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of capture group `%s' previously declared at %s", sym.Name, alt.Pos))
					return nil
				}
			}
			for i, capref := range re.CapNames() {
				if capref != "" {
					sym := NewSymbol(capref, CaprefSymbol, n.Pos())
					sym.Type = inferCaprefType(re, i)
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

func isError(t Type) bool {
	if o, ok := t.(*TypeOperator); ok {
		if o.Name == "Error" {
			return true
		}
	}
	return false
}

func (c *checker) VisitAfter(node astNode) {
	switch n := node.(type) {
	case *stmtlistNode:
		c.scope = n.s.Parent

	case *condNode:
		c.scope = n.s.Parent

	case *binaryExprNode:
		var rType Type
		lT := n.lhs.Type()
		glog.Infof("lhs is %v: %v", n.lhs, lT)
		if isError(lT) {
			n.SetType(Error)
			return
		}
		rT := n.rhs.Type()
		glog.Infof("rhs is %v; %v", n.rhs, rT)
		if isError(rT) {
			n.SetType(Error)
			return
		}
		switch n.op {
		case DIV, MOD, MUL, MINUS, PLUS, POW:
			// Numeric
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = NewTypeVariable()
			opType := Function(rType, rType, rType)
			exprType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, opType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}
		case SHL, SHR, AND, OR, XOR, NOT:
			// bitwise
			// O ⊢ e1 :Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			rType = Int
			opType := Function(rType, rType, rType)
			exprType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, opType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise op %q, got %s and %s", n.op, lT, rT))
				n.SetType(Error)
				return
			}
		case LT, GT, LE, GE, EQ, NE:
			// comparable
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = Int
			t := NewTypeVariable()
			// rType = NewTypeVariable()
			opType := Function(t, t, rType)
			exprType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, opType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}
			rType = Int
		case ASSIGN:
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr
			// ⇒ O ⊢ e : Tl
			// glog.Infof("Tl: %v TR: %v", Tl, Tr)
			rType = NewTypeVariable()
			opType := Function(rType, NewTypeVariable(), rType)
			exprType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, opType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(Error)
				return
			}
		default:
			if lT != rT {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch between lhs (%v) and rhs (%v) for op %q", lT, rT, n.op))
				n.SetType(Error)
				return
			}
			rType = lT
		}
		n.SetType(rType)

	case *unaryExprNode:
		t := n.expr.Type()
		if isError(t) {
			n.SetType(Error)
			return
		}
		switch n.op {
		case NOT:
			rType := Int
			err := Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(Error)
				return
			}
			n.SetType(rType)
		case INC:
			rType := Int
			glog.Infof("expr %q type is %q", n.expr, t)
			err := Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(Error)
				return
			}
			n.SetType(rType)
		default:
			// TODO: error
			c.errors.Add(n.Pos(), fmt.Sprintf("unknown unary expr %v", n))
			n.SetType(Error)
			return
		}

	case *indexedExprNode:
		glog.Infof("n.lhs: %#v", n.lhs)
		lType := n.lhs.Type()
		if isError(lType) {
			n.SetType(Error)
			return
		}
		iType := n.index.Type()
		glog.Infof("index types: %v [ %v ]", lType, iType)
		if isError(iType) {
			n.SetType(Error)
			return
		}
		switch lT := lType.(type) {
		case *TypeOperator:
			if len(lT.Args) <= 1 {
				c.errors.Add(n.Pos(), fmt.Sprintf("Too many keys for metric"))
				n.SetType(Error)
				return
			}
			glog.Infof("left, right: %v %v", lT.Args[0], iType)
			err := Unify(lT.Args[0], iType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(Error)
				return
			}
			glog.Infof("setting tupe %v", lT.Args[1:])
			n.SetType(Function(lT.Args[1:]...))
		case *TypeVariable:
			n.SetType(lT)
		}
		glog.Infof("expr %q is now %q", n, n.Type())

	case *builtinNode:
		types := []Type{}
		if args, ok := n.args.(*exprlistNode); ok {
			for _, arg := range args.children {
				types = append(types, arg.Type())
			}
		}
		rType := NewTypeVariable()
		types = append(types, rType)

		fn := Function(types...)
		err := Unify(n.Type(), fn)
		if err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.name, err))
			// TODO: put type on expression tree
			// n.SetType(Error)
			return
		}
	}
}
