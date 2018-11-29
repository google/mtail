// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/symtab"
	"github.com/google/mtail/internal/vm/types"
)

// checker holds data for a semantic checker
type checker struct {
	scope *symtab.Scope // the current scope

	decoScopes []*symtab.Scope // A stack of scopes used for resolving symbols in decorated nodes

	errors ErrorList
}

// Check performs a semantic check of the astNode, and returns a potentially
// modified astNode and either a list of errors found, or nil if the program is
// semantically valid.  At the completion of Check, the symbol table and type
// annotation are also complete.
func Check(node astNode) (astNode, error) {
	c := &checker{}
	node = Walk(c, node)
	if len(c.errors) > 0 {
		return node, c.errors
	}
	return node, nil
}

// VisitBefore performs most of the symbol table construction, so that symbols
// are guaranteed to exist before their use.
func (c *checker) VisitBefore(node astNode) (Visitor, astNode) {
	switch n := node.(type) {

	case *StmtList:
		n.Scope = symtab.NewScope(c.scope)
		c.scope = n.Scope
		return c, n

	case *Cond:
		n.s = symtab.NewScope(c.scope)
		c.scope = n.s
		return c, n

	case *CaprefNode:
		if n.sym == nil {
			if sym := c.scope.Lookup(n.name, symtab.CaprefSymbol); sym == nil {
				msg := fmt.Sprintf("Capture group `$%s' was not defined by a regular expression visible to this scope.", n.name)
				if n.isNamed {
					msg = fmt.Sprintf("%s\n\tTry using `(?P<%s>...)' to name the capture group.", msg, n.name)
				} else {
					msg = fmt.Sprintf("%s\n\tCheck that there are at least %s pairs of parentheses.", msg, n.name)
				}
				c.errors.Add(n.Pos(), msg)
				return nil, n
			} else {
				sym.Used = true
				n.sym = sym
			}
		}
		return c, n

	case *DeclNode:
		n.sym = symtab.NewSymbol(n.name, symtab.VarSymbol, n.Pos())
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.name, alt.Pos))
			return nil, n
		}
		var rType types.Type
		switch n.kind {
		case metrics.Counter, metrics.Gauge, metrics.Timer:
			// TODO(jaq): This should be a numeric type, unless we want to
			// enforce rules like "Counter can only be Int."
			rType = types.NewTypeVariable()
		case metrics.Text:
			rType = types.String
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("internal compiler error: unrecognised Kind %v for declNode %v", n.kind, n))
			return nil, n
		}
		if len(n.keys) > 0 {
			// One type per key
			keyTypes := make([]types.Type, 0, len(n.keys))
			for i := 0; i < len(n.keys); i++ {
				keyTypes = append(keyTypes, types.NewTypeVariable())
			}
			// and one for the value.
			keyTypes = append(keyTypes, rType)
			n.sym.Type = types.Dimension(keyTypes...)
		} else {
			n.sym.Type = rType
		}
		return c, n

	case *Id:
		if n.sym == nil {
			if sym := c.scope.Lookup(n.name, symtab.VarSymbol); sym != nil {
				glog.V(2).Infof("found sym %v", sym)
				sym.Used = true
				n.sym = sym
			} else if sym := c.scope.Lookup(n.name, symtab.PatternSymbol); sym != nil {
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
				return nil, n
			}
		}
		return c, n

	case *DecoDefNode:
		n.sym = symtab.NewSymbol(n.name, symtab.DecoSymbol, n.Pos())
		(*n.sym).Binding = n
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of decorator `%s' previously declared at %s", n.name, alt.Pos))
			return nil, n
		}
		return c, n

	case *DecoNode:
		if sym := c.scope.Lookup(n.name, symtab.DecoSymbol); sym != nil {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.name))
				return nil, n
			}
			sym.Used = true
			n.def = sym.Binding.(*DecoDefNode)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.name, n.name))
			return nil, n
		}
		n.scope = symtab.NewScope(c.scope)
		// Insert all of n.def.scope into this scope
		n.scope.CopyFrom(n.def.scope)
		c.scope = n.scope
		return c, n

	case *PatternFragmentDefNode:
		id, ok := n.id.(*Id)
		if !ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: no identifier attache to pattern fragment %#v", n))
			return nil, n
		}
		n.sym = symtab.NewSymbol(id.name, symtab.PatternSymbol, id.Pos())
		if alt := c.scope.Insert(n.sym); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redefinition of pattern constant `%s' previously defined at %s", id.name, alt.Pos))
			return nil, n
		}
		n.sym.Binding = n
		n.sym.Type = types.Pattern
		return c, n

	case *DelNode:
		n.n = Walk(c, n.n)
		return c, n

	}
	return c, node
}

// checkSymbolUsage emits errors if any eligible symbols in the current scope
// are not marked as used.
func (c *checker) checkSymbolUsage() {
	for _, sym := range c.scope.Symbols {
		if !sym.Used {
			// Users don't have control over the patterns given from decorators
			// so this should never be an error; but it can be useful to know
			// if a program is doing unnecessary work.
			if sym.Kind == symtab.CaprefSymbol {
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
func (c *checker) VisitAfter(node astNode) astNode {
	switch n := node.(type) {
	case *StmtList:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.Scope.Parent
		return n

	case *Cond:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.s.Parent
		return n

	case *DecoNode:
		// Don't check symbol usage here because the decorator is only partially defined.
		c.scope = n.scope.Parent
		return n

	case *NextNode:
		// Put the current scope on a decorator-specific scoe stack for unwinding
		c.decoScopes = append(c.decoScopes, c.scope)
		return n

	case *DecoDefNode:
		// Pop a decorator scope off the stack from the enclosed nextNode.
		last := len(c.decoScopes) - 1
		n.scope = c.decoScopes[last]
		c.decoScopes = c.decoScopes[:last]
		return n

	case *BinaryExpr:
		var rType types.Type
		lT := n.lhs.Type()
		switch {
		case types.IsErrorType(lT):
			n.SetType(types.Error)
			return n
		}
		rT := n.rhs.Type()
		switch {
		case types.IsErrorType(rT):
			n.SetType(types.Error)
			return n
		}
		switch n.op {
		case DIV, MOD, MUL, MINUS, PLUS, POW:
			// Numeric
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)
			rType = types.LeastUpperBound(lT, rT)
			if types.IsErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have no common type", lT, rT))
				n.SetType(rType)
				return n
			}
			// astType is the type signature of the ast expression
			astType := types.Function(lT, rT, rType)

			t := types.NewTypeVariable()
			// exprType is the type signature of this expression
			exprType := types.Function(t, t, t)
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(types.Error)
				return n
			}
			// Implicit type conversion for non-comparisons, promoting each
			// half to the return type of the op.
			if !types.Equals(rType, lT) {
				conv := &ConvNode{n: n.lhs, typ: rType}
				n.lhs = conv
			}
			if !types.Equals(rType, rT) {
				conv := &ConvNode{n: n.rhs, typ: rType}
				n.rhs = conv
			}

		case SHL, SHR, BITAND, BITOR, XOR, NOT:
			// bitwise
			// O ⊢ e1 :Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			rType = types.Int
			exprType := types.Function(rType, rType, rType)
			astType := types.Function(lT, rT, types.NewTypeVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise op %q, got %s and %s", n.op, lT, rT))
				n.SetType(types.Error)
				return n
			}
		case LT, GT, LE, GE, EQ, NE, AND, OR:
			// comparable, logical
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : Bool
			rType = types.Bool
			if types.IsErrorType(rType) {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %q and %q have no common type", lT, rT))
				n.SetType(rType)
				return n
			}
			astType := types.Function(lT, rT, rType)

			t := types.LeastUpperBound(lT, rT)
			exprType := types.Function(t, t, types.Bool)
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}
			// Promote types if the ast types are not the same as the expression type.
			if !types.Equals(t, lT) {
				conv := &ConvNode{n: n.lhs, typ: t}
				n.lhs = conv
				glog.V(2).Infof("Emitting convnode %+v", conv)
			}
			if !types.Equals(t, rT) {
				conv := &ConvNode{n: n.rhs, typ: t}
				n.rhs = conv
				glog.V(2).Infof("Emitting convnode %+v", conv)
			}

		case ASSIGN, ADD_ASSIGN:
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tr <= Tl
			// ⇒ O ⊢ e : Tl
			glog.V(2).Infof("lt %q, rt %q", lT, rT)
			rType = lT
			// TODO(jaq): the rT <= lT relationship is not correctly encoded here.
			t := types.LeastUpperBound(lT, rT)
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(types.Error)
				return n
			}
			switch v := n.lhs.(type) {
			case *Id:
				v.lvalue = true
			case *IndexedExpr:
				v.lhs.(*Id).lvalue = true
			}

		case CONCAT:
			rType = types.Pattern
			exprType := types.Function(rType, rType, rType)
			astType := types.Function(lT, rT, types.NewTypeVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}

		case MATCH, NOT_MATCH:
			rType = types.Bool
			exprType := types.Function(types.NewTypeVariable(), types.Pattern, rType)
			astType := types.Function(lT, rT, types.NewTypeVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator %v in node %#v", n.op, n))
			n.SetType(types.Error)
			return n
		}
		n.SetType(rType)
		return n

	case *UnaryExpr:
		t := n.expr.Type()
		switch {
		case types.IsErrorType(t):
			n.SetType(types.Error)
			return n
		}
		switch n.op {
		case NOT:
			rType := types.Int
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)
		case INC, DEC:
			rType := types.Int
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("%s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)
			switch v := n.expr.(type) {
			case *Id:
				v.lvalue = true
			case *IndexedExpr:
				v.lhs.(*Id).lvalue = true
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("unknown unary op %s in expr %#v", lexeme(n.op), n))
			n.SetType(types.Error)
			return n
		}
		return n

	case *ExprList:
		argTypes := []types.Type{}
		for _, arg := range n.children {
			if types.IsErrorType(arg.Type()) {
				n.SetType(types.Error)
				return n
			}
			argTypes = append(argTypes, arg.Type())
		}
		n.SetType(types.Dimension(argTypes...))
		return n

	case *IndexedExpr:
		argTypes := []types.Type{}
		if args, ok := n.index.(*ExprList); ok {
			for _, arg := range args.children {
				if types.IsErrorType(arg.Type()) {
					n.SetType(types.Error)
					return n
				}
				argTypes = append(argTypes, arg.Type())
			}
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected %v", n.index))
			n.SetType(types.Error)
			return n
		}

		switch v := n.lhs.(type) {
		case *Id:
			if v.sym == nil {
				// undefined, already caught
				n.SetType(types.Error)
				return n
			}
			// ok
			if t, ok := v.Type().(*types.TypeOperator); ok && types.IsDimension(t) {
				glog.V(1).Infof("Our idNode is a dimension type")
			} else {
				if len(argTypes) > 0 {
					glog.V(1).Infof("Our idNode is not a dimension type")
					n.SetType(types.Error)
					c.errors.Add(n.Pos(), fmt.Sprintf("Index taken on unindexable expression"))
				} else {
					n.SetType(v.Type())
				}
				return n
			}
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Index taken on unindexable expression"))
			n.SetType(types.Error)
			return n
		}

		rType := types.NewTypeVariable()
		argTypes = append(argTypes, rType)
		astType := types.Dimension(argTypes...)
		fresh := n.lhs.Type()
		err := types.Unify(fresh, astType)
		if err != nil {
			exprType, ok := n.lhs.Type().(*types.TypeOperator)
			if !ok {
				c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected lhs type %v", n.lhs.Type()))
				n.SetType(types.Error)
				return n
			}
			switch {
			case len(exprType.Args) > len(astType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Not enough keys for indexed expression: expecting %d, received %d", len(exprType.Args)-1, len(astType.Args)-1))
				n.SetType(types.Error)
				return n
			case len(exprType.Args) < len(astType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Too many keys for indexed expression: expecting %d, received %d.", len(exprType.Args)-1, len(astType.Args)-1))
			default:
				c.errors.Add(n.Pos(), fmt.Sprintf("Index lookup expression %s", err))
			}
			n.SetType(types.Error)
			return n
		}
		n.SetType(rType)
		return n

	case *BuiltinNode:
		typs := []types.Type{}
		if args, ok := n.args.(*ExprList); ok {
			for _, arg := range args.children {
				typs = append(typs, arg.Type())
			}
		}
		rType := types.NewTypeVariable()
		typs = append(typs, rType)

		fn := types.Function(typs...)
		fresh := types.FreshType(types.Builtins[n.name])
		err := types.Unify(fresh, fn)
		if err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.name, err))
			n.SetType(types.Error)
			return n
		}
		n.SetType(rType)

		switch n.name {
		case "strptime":
			// Second argument to strptime is the format string.  If it is
			// defined at compile time, we can verify it can be use as a format
			// string by parsing itself.
			if f, ok := n.args.(*ExprList).children[1].(*StringConst); ok {
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
					n.SetType(types.Error)
					return n
				}
			}
		}
		return n

	case *PatternExpr:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n = Walk(pe, n).(*PatternExpr)
		if pe.pattern == "" {
			return n
		}
		n.pattern = pe.pattern
		c.checkRegex(pe.pattern, n)
		return n

	case *PatternFragmentDefNode:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n.expr = Walk(pe, n.expr)
		if pe.pattern == "" {
			return n
		}
		n.pattern = pe.pattern
		return n

	case *DelNode:
		n.n.(*IndexedExpr).lhs.(*Id).lvalue = true
		return n

	}

	return node
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
			sym := symtab.NewSymbol(fmt.Sprintf("%d", i), symtab.CaprefSymbol, n.Pos())
			sym.Type = types.InferCaprefType(reAst, i)
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
	scope   *symtab.Scope
	errors  *ErrorList
	pattern string
}

func (p *patternEvaluator) VisitBefore(n astNode) (Visitor, astNode) {
	switch v := n.(type) {
	case *BinaryExpr:
		if v.op != CONCAT {
			p.errors.Add(v.Pos(), fmt.Sprintf("internal error: Invalid operator in concatenation: %v", v))
			return nil, n
		}
		return p, v
	case *PatternConst:
		p.pattern += v.pattern
		return p, v
	case *Id:
		// Already looked up sym, if still nil undefined.
		if v.sym == nil {
			return nil, n
		}
		idPattern := v.sym.Binding.(*PatternFragmentDefNode).pattern
		if idPattern == "" {
			idEvaluator := &patternEvaluator{scope: p.scope}
			n = Walk(idEvaluator, v.sym.Binding.(*PatternFragmentDefNode))
			idPattern = idEvaluator.pattern
		}
		p.pattern += idPattern
		return p, v
	}
	return p, n
}

func (p *patternEvaluator) VisitAfter(n astNode) astNode {
	return n
}
