// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"strings"
	"time"

	"github.com/golang/glog"
)

// checker holds data for a semantic checker
type checker struct {
	scope *Scope // the current scope

	decoScopes []*Scope // A stack of scopes used for resolving symbols in decorated nodes

	errors ErrorList
}

// Check performs a semantic check of the astNode, and returns a list of errors
// found, or nil if the program is semantically valid.  At the completion of
// Check, the symbol table and type annotation are also complete.
func Check(node astNode) error {
	c := &checker{}
	Walk(c, node)
	if len(c.errors) > 0 {
		return c.errors
	}
	return nil
}

// VisitBefore performs most of the symbol table construction, so that symbols
// are guaranteed to exist before their use.
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
			if sym := c.scope.Lookup(n.name, CaprefSymbol); sym == nil {
				msg := fmt.Sprintf("Capture group `$%s' was not defined by a regular expression visible to this scope.", n.name)
				if n.isNamed {
					msg = fmt.Sprintf("%s\n\tTry using `(?P<%s>...)' to name the capture group.", msg, n.name)
				} else {
					msg = fmt.Sprintf("%s\n\tCheck that there are at least %s pairs of parentheses.", msg, n.name)
				}
				c.errors.Add(n.Pos(), msg)
				return nil
			} else {
				sym.Used = true
				n.sym = sym
			}
		}

	case *declNode:
		n.sym = NewSymbol(n.name, VarSymbol, n.Pos())
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.name, alt.Pos))
			return nil
		}
		if len(n.keys) > 0 {
			// One type per key and one for the value.
			keyTypes := make([]Type, 0, len(n.keys)+1)
			for i := 0; i <= len(n.keys); i++ {
				keyTypes = append(keyTypes, NewTypeVariable())
			}
			n.sym.Type = Dimension(keyTypes...)
		} else {
			n.sym.Type = NewTypeVariable()
		}

	case *idNode:
		if n.sym == nil {
			if sym := c.scope.Lookup(n.name, VarSymbol); sym != nil {
				glog.V(2).Infof("found sym %v", sym)
				sym.Used = true
				n.sym = sym
			} else if sym := c.scope.Lookup(n.name, PatternSymbol); sym != nil {
				glog.V(2).Infof("Found Sym %v", sym)
				sym.Used = true
				n.sym = sym
			} else {
				// Apply a terribly bad heuristic to choose a suggestion.
				sug := fmt.Sprintf("Try adding `counter %s' to the top of the program.", n.name)
				if n.name == strings.ToUpper(n.name) {
					// If the string is all uppercase, pretend it was a const
					// pattern because that's what the docs do.
					sug = fmt.Sprintf("Try adding `const %s /.../' earlier in the program.", n.name)
				}
				c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\t%s", n.name, sug))
				return nil
			}
		}

	case *decoDefNode:
		n.sym = NewSymbol(n.name, DecoSymbol, n.Pos())
		(*n.sym).Binding = n
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of decorator `%s' previously declared at %s", n.name, alt.Pos))
			return nil
		}

	case *decoNode:
		if sym := c.scope.Lookup(n.name, DecoSymbol); sym != nil {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.name))
				return nil
			}
			sym.Used = true
			n.def = sym.Binding.(*decoDefNode)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.name, n.name))
			return nil
		}
		n.scope = NewScope(c.scope)
		// Insert all of n.def.scope into this scope
		n.scope.CopyFrom(n.def.scope)
		c.scope = n.scope

	case *patternFragmentDefNode:
		id, ok := n.id.(*idNode)
		if !ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: no identifier attache to pattern fragment %#v", n))
			return nil
		}
		n.sym = NewSymbol(id.name, PatternSymbol, id.Pos())
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redefinition of pattern constant `%s' previously defined at %s", id.name, alt.Pos))
			return nil
		}
		n.sym.Binding = n
		n.sym.Type = Pattern

	case *delNode:
		Walk(c, n.n)

	}
	return c
}

// checkSymbolUsage emits errors if any eligible symbols in the current scope
// are not marked as used.
func (c *checker) checkSymbolUsage() {
	for _, sym := range c.scope.Symbols {
		if !sym.Used {
			// Users don't have control over the patterns given from decorators
			// so this should never be an error; but it can be useful to know
			// if a program is doing unnecessary work.
			if sym.Kind == CaprefSymbol {
				if sym.Addr == 0 {
					// Don't warn about the zeroth capture group; it's not user-defined.
					continue
				}
				glog.Infof("declaration of capture group reference `%s' at %s appears to be unused", sym.Name, sym.Pos)
				continue
			}
			c.errors.Add(sym.Pos, fmt.Sprintf("Declaration of %s `%s' is never used", sym.Kind, sym.Name))
		}
	}
}

// VisitAfter performs the type annotation and checking, once the child nodes of
// expressions have been annotated and checked.
func (c *checker) VisitAfter(node astNode) {
	switch n := node.(type) {
	case *stmtlistNode:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.s.Parent

	case *condNode:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.s.Parent

	case *decoNode:
		c.checkSymbolUsage()
		c.scope = n.scope.Parent

	case *nextNode:
		// Put the current scope on a decorator-specific scoe stack for unwinding
		c.decoScopes = append(c.decoScopes, c.scope)

	case *decoDefNode:
		// Pop a decorator scope off the stack from the enclosed nextNode.
		last := len(c.decoScopes) - 1
		n.scope = c.decoScopes[last]
		c.decoScopes = c.decoScopes[:last]

	case *binaryExprNode:
		var rType Type
		lT := n.lhs.Type()
		switch {
		case isErrorType(lT):
			n.SetType(Error)
			return
		}
		rT := n.rhs.Type()
		switch {
		case isErrorType(rT):
			n.SetType(Error)
			return
		}
		switch n.op {
		case DIV, MOD, MUL, MINUS, PLUS, POW:
			// Numeric
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = LeastUpperBound(lT, rT)
			if isErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have no common type", lT, rT))
				n.SetType(rType)
				return
			}
			// astType is the type signature of the ast expression
			astType := Function(lT, rT, rType)

			t := NewTypeVariable()
			// exprType is the type signature of this expression
			exprType := Function(t, t, t)
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(Error)
				return
			}
			// Implicit type conversion for non-comparisons, promoting each
			// half to the return type of the op.
			if !Equals(rType, lT) {
				conv := &convNode{n: n.lhs, typ: rType}
				n.lhs = conv
			}
			if !Equals(rType, rT) {
				conv := &convNode{n: n.rhs, typ: rType}
				n.rhs = conv
			}

		case SHL, SHR, BITAND, BITOR, XOR, NOT:
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
		case LT, GT, LE, GE, EQ, NE, AND, OR:
			// comparable, logical
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : Bool
			rType = Bool
			if isErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have no common type", lT, rT))
				n.SetType(rType)
				return
			}
			astType := Function(lT, rT, rType)

			t := LeastUpperBound(lT, rT)
			exprType := Function(t, t, Bool)
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}
			// Promote types if the ast types are not the same as the expression type.
			if !Equals(t, lT) {
				conv := &convNode{n: n.lhs, typ: t}
				n.lhs = conv
				glog.Infof("Emitting convnode %+v", conv)
			}
			if !Equals(t, rT) {
				conv := &convNode{n: n.rhs, typ: t}
				n.rhs = conv
				glog.Infof("Emitting convnode %+v", conv)
			}

		case ASSIGN, ADD_ASSIGN:
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tr <= Tl
			// ⇒ O ⊢ e : Tl
			rType = lT
			err := Unify(rType, rT)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(Error)
				return
			}
			switch v := n.lhs.(type) {
			case *idNode:
				v.lvalue = true
			case *indexedExprNode:
				v.lhs.(*idNode).lvalue = true
			}

		case CONCAT:
			rType = Pattern
			exprType := Function(rType, rType, rType)
			astType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}

		case MATCH, NOT_MATCH:
			rType = Bool
			exprType := Function(NewTypeVariable(), Pattern, rType)
			astType := Function(lT, rT, NewTypeVariable())
			err := Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(Error)
				return
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator %v in node %#v", n.op, n))
			n.SetType(Error)
			return
		}
		n.SetType(rType)

	case *unaryExprNode:
		t := n.expr.Type()
		switch {
		case isErrorType(t):
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
			err := Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("%s", err))
				n.SetType(Error)
				return
			}
			n.SetType(rType)
			switch v := n.expr.(type) {
			case *idNode:
				v.lvalue = true
			case *indexedExprNode:
				v.lhs.(*idNode).lvalue = true
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("unknown unary expr %v", n))
			n.SetType(Error)
			return
		}

	case *exprlistNode:
		argTypes := []Type{}
		for _, arg := range n.children {
			if isErrorType(arg.Type()) {
				n.SetType(Error)
				return
			}
			argTypes = append(argTypes, arg.Type())
		}
		n.SetType(Dimension(argTypes...))

	case *indexedExprNode:
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

		switch v := n.lhs.(type) {
		case *idNode:
			if v.sym == nil {
				// undefined, already caught
				n.SetType(Error)
				return
			}
			// ok
			if t, ok := v.Type().(*TypeOperator); ok && IsDimension(t) {
				glog.V(1).Infof("Our idNode is a dimension type")
			} else {
				if len(argTypes) > 0 {
					glog.V(1).Infof("Our idNode is not a dimension type")
					n.SetType(Error)
					c.errors.Add(n.Pos(), fmt.Sprintf("Index taken on unindexable expression"))
				} else {
					n.SetType(v.Type())
				}
				return
			}
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Index taken on unindexable expression"))
			n.SetType(Error)
			return
		}

		rType := NewTypeVariable()
		argTypes = append(argTypes, rType)
		astType := Dimension(argTypes...)
		fresh := n.lhs.Type()
		err := Unify(fresh, astType)
		if err != nil {
			exprType, ok := n.lhs.Type().(*TypeOperator)
			if !ok {
				c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected lhs type %v", n.lhs.Type()))
				n.SetType(Error)
				return
			}
			switch {
			case len(exprType.Args) > len(astType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Not enough keys for indexed expression: expecting %d, received %d", len(exprType.Args)-1, len(astType.Args)-1))
				n.SetType(Error)
				return
			case len(exprType.Args) < len(astType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Too many keys for indexed expression: expecting %d, received %d.", len(exprType.Args)-1, len(astType.Args)-1))
			default:
				c.errors.Add(n.Pos(), fmt.Sprintf("Index lookup expression %s", err))
			}
			n.SetType(Error)
			return
		}
		n.SetType(rType)

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
		fresh := FreshType(Builtins[n.name])
		err := Unify(fresh, fn)
		if err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.name, err))
			n.SetType(Error)
			return
		}
		n.SetType(rType)

		switch n.name {
		case "strptime":
			// Second argument to strptime is the format string.  If it is
			// defined at compile time, we can verify it can be use as a format
			// string by parsing itself.
			if f, ok := n.args.(*exprlistNode).children[1].(*stringConstNode); ok {
				// Layout strings can contain an underscore to indicate a digit
				// field if the layout field can contain two digits; but they
				// won't parse themselves.  Zulu Timezones in the layout need
				// to be converted to offset in the parsed time.
				timeStr := strings.Replace(strings.Replace(f.text, "_", "", -1), "Z", "+", -1)
				glog.V(2).Infof("time_str is %q", timeStr)
				_, err := time.Parse(f.text, timeStr)
				if err != nil {
					glog.Infof("time.Parse(%q, %q) failed: %s", f.text, timeStr, err)
					c.errors.Add(f.Pos(), fmt.Sprintf("invalid time format string %q\n\tRefer to the documentation at https://golang.org/pkg/time/#pkg-constants for advice.", f.text))
					n.SetType(Error)
					return
				}
			}
		}

	case *patternExprNode:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		Walk(pe, n)
		if pe.pattern == "" {
			return
		}
		n.pattern = pe.pattern
		c.checkRegex(pe.pattern, n)

	case *patternFragmentDefNode:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		Walk(pe, n.expr)
		if pe.pattern == "" {
			return
		}
		n.pattern = pe.pattern

	case *delNode:
		n.n.(*indexedExprNode).lhs.(*idNode).lvalue = true
	}
}

// checkRegex is a helper method to compile and check a regular expression, and
// to generate its capture groups as symbols.
func (c *checker) checkRegex(pattern string, n astNode) {
	if reAst, err := syntax.Parse(pattern, syntax.Perl); err == nil {
		// We reserve the names of the capturing groups as declarations
		// of those symbols, so that future CAPREF tokens parsed can
		// retrieve their value.  By recording them in the symbol table, we
		// can warn the user about unknown capture group references.
		for i, capref := range reAst.CapNames() {
			sym := NewSymbol(fmt.Sprintf("%d", i), CaprefSymbol, n.Pos())
			sym.Type = inferCaprefType(reAst, i)
			sym.Binding = n
			sym.Addr = i
			if alt := c.scope.Insert(sym); alt != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of capture group `%s' previously declared at %s", sym.Name, alt.Pos))
				// No return, let this loop collect all errors
			}
			if capref != "" {
				sym.Name = capref
				if alt := c.scope.InsertAlias(sym, capref); alt != nil {
					c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of capture group `%s' previously declared at %s", sym.Name, alt.Pos))
					// No return, let this loop collect all errors
				}
			}
		}
	} else {
		c.errors.Add(n.Pos(), err.Error())
		return
	}
}

// patternEvaluator is a helper that performs concatenation of pattern
// fragments so that they can be compiled as whole regular expression patterns.
type patternEvaluator struct {
	scope   *Scope
	errors  *ErrorList
	pattern string
}

func (p *patternEvaluator) VisitBefore(n astNode) Visitor {
	switch v := n.(type) {
	case *binaryExprNode:
		if v.op != CONCAT {
			p.errors.Add(v.Pos(), fmt.Sprintf("internal error: Invalid operator in concatenation: %v", v))
			return nil
		}
	case *patternConstNode:
		p.pattern += v.pattern
	case *idNode:
		// Already looked up sym, if still nil undefined.
		if v.sym == nil {
			return nil
		}
		idPattern := v.sym.Binding.(*patternFragmentDefNode).pattern
		if idPattern == "" {
			idEvaluator := &patternEvaluator{scope: p.scope}
			Walk(idEvaluator, v.sym.Binding.(*patternFragmentDefNode))
			idPattern = idEvaluator.pattern
		}
		p.pattern += idPattern
	}
	return p
}

func (p *patternEvaluator) VisitAfter(n astNode) {
}
