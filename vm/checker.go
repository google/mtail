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
		if len(n.keys) == 0 {
			n.sym.Type = NewTypeVariable()
		} else {
			// One type per key and one for the value.
			keyTypes := make([]Type, 0, len(n.keys)+1)
			for i := 0; i <= len(n.keys); i++ {
				keyTypes = append(keyTypes, NewTypeVariable())
			}
			n.sym.Type = Function(keyTypes...)
		}
		glog.Infof("Making type of %s now %s", n.name, n.sym.Type)

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

func isErrorType(t Type) bool {
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
		glog.Info("binExpr Node")
		var rType Type
		lT := n.lhs.Type()
		glog.Infof("lhs is %v: %v", n.lhs, lT)
		if isErrorType(lT) {
			n.SetType(Error)
			return
		}
		rT := n.rhs.Type()
		glog.Infof("rhs is %v; %v", n.rhs, rT)
		if isErrorType(rT) {
			n.SetType(Error)
			return
		}
		switch n.op {
		case DIV, MOD, MUL, MINUS, PLUS, POW:
			// Numeric
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			glog.Info("arith op")
			rType = LeastUpperBound(lT, rT)
			if isErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have nothing in common", lT, rT))
				n.SetType(Error)
				return
			}
			// astType is the type signature of the ast expression
			astType := Function(lT, rT, rType)

			t := NewTypeVariable()
			// exprType is the type signature of this expression
			exprType := Function(t, t, t)
			err := Unify(exprType, astType)
			glog.Infof("post unify of %v op is %v, %v", n, exprType, rType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(Error)
				return
			}
		case SHL, SHR, AND, OR, XOR, NOT:
			// bitwise
			// O ⊢ e1 :Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			rType = Int
			exprType := Function(rType, rType, rType)
			astType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise op %q, got %s and %s", n.op, lT, rT))
				n.SetType(Error)
				return
			}
		case LT, GT, LE, GE, EQ, NE:
			// comparable
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = LeastUpperBound(lT, rT)
			if isErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have nothing in common", lT, rT))
				n.SetType(Error)
				return
			}
			astType := Function(lT, rT, NewTypeVariable())

			t := NewTypeVariable()
			exprType := Function(t, t, Int)
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}
		case ASSIGN, ADD_ASSIGN:
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr
			// ⇒ O ⊢ e : Tl
			// glog.Infof("Tl: %v TR: %v", Tl, Tr)
			rType = NewTypeVariable()
			opType := Function(rType, NewTypeVariable(), rType)
			exprType := Function(lT, rT, NewTypeVariable())
			err := Unify(opType, exprType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(Error)
				return
			}
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator in node %v", n))
			n.SetType(Error)
			return
		}
		n.SetType(rType)

	case *unaryExprNode:
		t := n.expr.Type()
		if isErrorType(t) {
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
		// Rewrite chained index form to arg expr list form.
		switch v := n.lhs.(type) {
		case *idNode:
			// ok
		case *indexedExprNode:
			n.index.(*exprlistNode).children = append(v.index.(*exprlistNode).children, n.index.(*exprlistNode).children...)
			n.lhs = v.lhs
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Index taken on unindexable expression."))
			n.SetType(Error)
			return
		}

		glog.Infof("n.lhs: %#v", n.lhs)
		argTypes := []Type{}
		if args, ok := n.index.(*exprlistNode); ok {
			for _, arg := range args.children {
				if isErrorType(arg.Type()) {
					n.SetType(Error)
					return
				}
				argTypes = append(argTypes, arg.Type())
			}
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected %v", n.index))
			n.SetType(Error)
			return
		}
		glog.Infof("args is now %q", argTypes)
		rType := NewTypeVariable()
		argTypes = append(argTypes, rType)
		fn := Function(argTypes...)
		glog.Infof("We think this expr is of type %q", fn)
		glog.Infof("It shouldbe of type %q", n.lhs.Type())
		err := Unify(n.lhs.Type(), fn)
		if err != nil {
			glog.Info("that's an error")
			c.errors.Add(n.Pos(), fmt.Sprintf("index lookup: %s", err))
			n.SetType(Error)
			return
		}
		n.SetType(rType)
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
		fresh := FreshType(n.Type())
		err := Unify(fresh, fn)
		if err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.name, err))
			// TODO: put type on expression tree
			// n.SetType(Error)
			return
		}
	}
}
