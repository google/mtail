// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package checker

import (
	"fmt"
	"regexp/syntax"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/ast"
	"github.com/google/mtail/internal/vm/errors"
	"github.com/google/mtail/internal/vm/parser"
	"github.com/google/mtail/internal/vm/symbol"
	"github.com/google/mtail/internal/vm/types"
)

// checker holds data for a semantic checker
type checker struct {
	scope *symbol.Scope // the current scope

	decoScopes []*symbol.Scope // A stack of scopes used for resolving symbols in decorated nodes

	errors errors.ErrorList
}

// Check performs a semantic check of the astNode, and returns a potentially
// modified astNode and either a list of errors found, or nil if the program is
// semantically valid.  At the completion of Check, the symbol table and type
// annotation are also complete.
func Check(node ast.Node) (ast.Node, error) {
	c := &checker{}
	node = ast.Walk(c, node)
	if len(c.errors) > 0 {
		return node, c.errors
	}
	return node, nil
}

// VisitBefore performs most of the symbol table construction, so that symbols
// are guaranteed to exist before their use.
func (c *checker) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	switch n := node.(type) {

	case *ast.StmtList:
		n.Scope = symbol.NewScope(c.scope)
		c.scope = n.Scope
		return c, n

	case *ast.CondStmt:
		n.Scope = symbol.NewScope(c.scope)
		c.scope = n.Scope
		return c, n

	case *ast.CaprefTerm:
		if n.Symbol == nil {
			if sym := c.scope.Lookup(n.Name, symbol.CaprefSymbol); sym == nil {
				msg := fmt.Sprintf("Capture group `$%s' was not defined by a regular expression visible to this scope.", n.Name)
				if n.IsNamed {
					msg = fmt.Sprintf("%s\n\tTry using `(?P<%s>...)' to name the capture group.", msg, n.Name)
				} else {
					msg = fmt.Sprintf("%s\n\tCheck that there are at least %s pairs of parentheses.", msg, n.Name)
				}
				c.errors.Add(n.Pos(), msg)
				return nil, n
			} else {
				sym.Used = true
				n.Symbol = sym
			}
		}
		return c, n

	case *ast.VarDecl:
		n.Symbol = symbol.NewSymbol(n.Name, symbol.VarSymbol, n.Pos())
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.Name, alt.Pos))
			return nil, n
		}
		var rType types.Type
		switch n.Kind {
		case metrics.Counter, metrics.Gauge, metrics.Timer:
			// TODO(jaq): This should be a numeric type, unless we want to
			// enforce rules like "Counter can only be Int."
			rType = types.NewVariable()
		case metrics.Text:
			rType = types.String
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("internal compiler error: unrecognised Kind %v for declNode %v", n.Kind, n))
			return nil, n
		}
		if len(n.Keys) > 0 {
			// One type per key
			keyTypes := make([]types.Type, 0, len(n.Keys))
			for i := 0; i < len(n.Keys); i++ {
				keyTypes = append(keyTypes, types.NewVariable())
			}
			// and one for the value.
			keyTypes = append(keyTypes, rType)
			n.Symbol.Type = types.Dimension(keyTypes...)
		} else {
			n.Symbol.Type = rType
		}
		return c, n

	case *ast.IdTerm:
		if n.Symbol == nil {
			if sym := c.scope.Lookup(n.Name, symbol.VarSymbol); sym != nil {
				glog.V(2).Infof("found sym %v", sym)
				sym.Used = true
				n.Symbol = sym
			} else if sym := c.scope.Lookup(n.Name, symbol.PatternSymbol); sym != nil {
				glog.V(2).Infof("Found Sym %v", sym)
				sym.Used = true
				n.Symbol = sym
			} else {
				// Apply a terribly bad heuristic to choose a suggestion.
				sug := fmt.Sprintf("Try adding `counter %s' to the top of the program.", n.Name)
				if n.Name == strings.ToUpper(n.Name) {
					// If the string is all uppercase, pretend it was a const
					// pattern because that's what the docs do.
					sug = fmt.Sprintf("Try adding `const %s /.../' earlier in the program.", n.Name)
				}
				c.errors.Add(n.Pos(), fmt.Sprintf("Identifier `%s' not declared.\n\t%s", n.Name, sug))
				return nil, n
			}
		}
		return c, n

	case *ast.DecoDecl:
		n.Symbol = symbol.NewSymbol(n.Name, symbol.DecoSymbol, n.Pos())
		n.Symbol.Binding = n
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of decorator `%s' previously declared at %s", n.Name, alt.Pos))
			return nil, n
		}
		// Append a scope placeholder for the recursion into the block.  It has no parent, it'll be cloned when the decorator is instantiated.
		c.decoScopes = append(c.decoScopes, symbol.NewScope(nil))
		return c, n

	case *ast.DecoStmt:
		if sym := c.scope.Lookup(n.Name, symbol.DecoSymbol); sym != nil {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.Name))
				return nil, n
			}
			sym.Used = true
			n.Decl = sym.Binding.(*ast.DecoDecl)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `%s' not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.Name, n.Name))
			return nil, n
		}
		// Create a new scope for the decorator instantiation.
		n.Scope = symbol.NewScope(c.scope)
		// Clone the DecoDecl scope zygote into this scope.
		n.Scope.CopyFrom(n.Decl.Scope)
		c.scope = n.Scope
		return c, n

	case *ast.PatternFragment:
		id, ok := n.Id.(*ast.IdTerm)
		if !ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: no identifier attached to pattern fragment %#v", n))
			return nil, n
		}
		n.Symbol = symbol.NewSymbol(id.Name, symbol.PatternSymbol, id.Pos())
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redefinition of pattern constant `%s' previously defined at %s", id.Name, alt.Pos))
			return nil, n
		}
		n.Symbol.Binding = n
		n.Symbol.Type = types.Pattern
		return c, n

	case *ast.DelStmt:
		n.N = ast.Walk(c, n.N)
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
			if sym.Kind == symbol.CaprefSymbol {
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
func (c *checker) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.StmtList:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.Scope.Parent
		return n

	case *ast.CondStmt:
		c.checkSymbolUsage()
		// Pop the scope.
		c.scope = n.Scope.Parent
		return n

	case *ast.DecoStmt:
		// Don't check symbol usage here because the decorator is only partially defined.
		// Pop the scope.
		c.scope = n.Scope.Parent
		return n

	case *ast.NextStmt:
		// The last element in this list will be the empty stack created by the DecoDecl on the way in.
		last := len(c.decoScopes) - 1
		decoScope := c.decoScopes[last]
		if len(decoScope.Symbols) > 0 {
			c.errors.Add(n.Pos(), fmt.Sprintf("Can't use `next' statement twice in a decorator."))
		}
		// Merge the current scope into it.
		decoScope.CopyFrom(c.scope)
		return n

	case *ast.DecoDecl:
		// Pop the scope off the list, and insert it into this node.
		last := len(c.decoScopes) - 1
		decoScope := c.decoScopes[last]
		if len(decoScope.Symbols) == 0 {
			c.errors.Add(n.Pos(), fmt.Sprintf("No symbols found in decorator `@%s', try adding a `next' statement.", n.Name))
		}
		n.Scope = decoScope
		c.decoScopes = c.decoScopes[:last]
		return n

	case *ast.BinaryExpr:
		var rType types.Type
		lT := n.Lhs.Type()
		if types.IsErrorType(lT) {
			n.SetType(types.Error)
			return n
		}
		rT := n.Rhs.Type()
		if types.IsErrorType(rT) {
			n.SetType(types.Error)
			return n
		}
		switch n.Op {
		case parser.DIV, parser.MOD, parser.MUL, parser.MINUS, parser.PLUS, parser.POW:
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

			t := types.NewVariable()
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
				conv := &ast.ConvExpr{N: n.Lhs}
				conv.SetType(rType)
				n.Lhs = conv
			}
			if !types.Equals(rType, rT) {
				conv := &ast.ConvExpr{N: n.Rhs}
				conv.SetType(rType)
				n.Rhs = conv
			}

		case parser.SHL, parser.SHR, parser.BITAND, parser.BITOR, parser.XOR, parser.NOT:
			// bitwise
			// O ⊢ e1 :Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			rType = types.Int
			exprType := types.Function(rType, rType, rType)
			astType := types.Function(lT, rT, types.NewVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), err.Error())
				c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise op %q, got %s and %s", n.Op, lT, rT))
				n.SetType(types.Error)
				return n
			}
		case parser.LT, parser.GT, parser.LE, parser.GE, parser.EQ, parser.NE, parser.AND, parser.OR:
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
				conv := &ast.ConvExpr{N: n.Lhs}
				conv.SetType(t)
				n.Lhs = conv
				glog.V(2).Infof("Emitting convnode %+v", conv)
			}
			if !types.Equals(t, rT) {
				conv := &ast.ConvExpr{N: n.Rhs}
				conv.SetType(t)
				n.Rhs = conv
				glog.V(2).Infof("Emitting convnode %+v", conv)
			}

		case parser.ASSIGN, parser.ADD_ASSIGN:
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
			switch v := n.Lhs.(type) {
			case *ast.IdTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.Lhs.(*ast.IdTerm).Lvalue = true
			}

		case parser.CONCAT:
			rType = types.Pattern
			exprType := types.Function(rType, rType, rType)
			astType := types.Function(lT, rT, types.NewVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}

		case parser.MATCH, parser.NOT_MATCH:
			rType = types.Bool
			exprType := types.Function(types.NewVariable(), types.Pattern, rType)
			astType := types.Function(lT, rT, types.NewVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator %v in node %#v", n.Op, n))
			n.SetType(types.Error)
			return n
		}
		n.SetType(rType)
		return n

	case *ast.UnaryExpr:
		t := n.Expr.Type()
		if types.IsErrorType(t) {
			n.SetType(types.Error)
			return n
		}
		switch n.Op {
		case parser.NOT:
			rType := types.Int
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)
		case parser.INC, parser.DEC:
			rType := types.Int
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("%s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)
			switch v := n.Expr.(type) {
			case *ast.IdTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.Lhs.(*ast.IdTerm).Lvalue = true
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("unknown unary op %s in expr %#v", parser.Kind(n.Op), n))
			n.SetType(types.Error)
			return n
		}
		return n

	case *ast.ExprList:
		argTypes := []types.Type{}
		for _, arg := range n.Children {
			if types.IsErrorType(arg.Type()) {
				n.SetType(types.Error)
				return n
			}
			argTypes = append(argTypes, arg.Type())
		}
		n.SetType(types.Dimension(argTypes...))
		return n

	case *ast.IndexedExpr:
		argTypes := []types.Type{}
		if args, ok := n.Index.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				if types.IsErrorType(arg.Type()) {
					n.SetType(types.Error)
					return n
				}
				argTypes = append(argTypes, arg.Type())
			}
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected %v", n.Index))
			n.SetType(types.Error)
			return n
		}

		switch v := n.Lhs.(type) {
		case *ast.IdTerm:
			if v.Symbol == nil {
				// undefined, already caught
				n.SetType(types.Error)
				return n
			}
			// ok
			if t, ok := v.Type().(*types.Operator); ok && types.IsDimension(t) {
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

		rType := types.NewVariable()
		argTypes = append(argTypes, rType)
		astType := types.Dimension(argTypes...)
		fresh := n.Lhs.Type()
		err := types.Unify(fresh, astType)
		if err != nil {
			exprType, ok := n.Lhs.Type().(*types.Operator)
			if !ok {
				c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected lhs type %v", n.Lhs.Type()))
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

	case *ast.BuiltinExpr:
		typs := []types.Type{}
		if args, ok := n.Args.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				typs = append(typs, arg.Type())
			}
		}
		rType := types.NewVariable()
		typs = append(typs, rType)

		fn := types.Function(typs...)
		fresh := types.FreshType(types.Builtins[n.Name])
		err := types.Unify(fresh, fn)
		if err != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.Name, err))
			n.SetType(types.Error)
			return n
		}
		n.SetType(rType)

		if n.Name == "strptime" {
			// Second argument to strptime is the format string.  If it is
			// defined at compile time, we can verify it can be use as a format
			// string by parsing itself.
			if f, ok := n.Args.(*ast.ExprList).Children[1].(*ast.StringLit); ok {
				// Layout strings can contain an underscore to indicate a digit
				// field if the layout field can contain two digits; but they
				// won't parse themselves.  Zulu Timezones in the layout need
				// to be converted to offset in the parsed time.
				timeStr := strings.Replace(strings.Replace(f.Text, "_", "", -1), "Z", "+", -1)
				glog.V(2).Infof("time_str is %q", timeStr)
				_, err := time.Parse(f.Text, timeStr)
				if err != nil {
					glog.Infof("time.Parse(%q, %q) failed: %s", f.Text, timeStr, err)
					c.errors.Add(f.Pos(), fmt.Sprintf("invalid time format string %q\n\tRefer to the documentation at https://golang.org/pkg/time/#pkg-constants for advice.", f.Text))
					n.SetType(types.Error)
					return n
				}
			}
		}
		return n

	case *ast.PatternExpr:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n = ast.Walk(pe, n).(*ast.PatternExpr)
		if pe.pattern == "" {
			return n
		}
		n.Pattern = pe.pattern
		c.checkRegex(pe.pattern, n)
		return n

	case *ast.PatternFragment:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n.Expr = ast.Walk(pe, n.Expr)
		if pe.pattern == "" {
			return n
		}
		n.Pattern = pe.pattern
		return n

	case *ast.DelStmt:
		n.N.(*ast.IndexedExpr).Lhs.(*ast.IdTerm).Lvalue = true
		return n

	}

	return node
}

// checkRegex is a helper method to compile and check a regular expression, and
// to generate its capture groups as symbols.
func (c *checker) checkRegex(pattern string, n ast.Node) {
	if reAst, err := syntax.Parse(pattern, syntax.Perl); err == nil {
		// We reserve the names of the capturing groups as declarations
		// of those symbols, so that future CAPREF tokens parsed can
		// retrieve their value.  By recording them in the symbol table, we
		// can warn the user about unknown capture group references.
		for i, capref := range reAst.CapNames() {
			sym := symbol.NewSymbol(fmt.Sprintf("%d", i), symbol.CaprefSymbol, n.Pos())
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
	scope   *symbol.Scope
	errors  *errors.ErrorList
	pattern string
}

func (p *patternEvaluator) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	switch v := n.(type) {
	case *ast.BinaryExpr:
		if v.Op != parser.CONCAT {
			p.errors.Add(v.Pos(), fmt.Sprintf("internal error: Invalid operator in concatenation: %v", v))
			return nil, n
		}
		return p, v
	case *ast.PatternLit:
		p.pattern += v.Pattern
		return p, v
	case *ast.IdTerm:
		// Already looked up sym, if still nil undefined.
		if v.Symbol == nil {
			return nil, n
		}
		idPattern := v.Symbol.Binding.(*ast.PatternFragment).Pattern
		if idPattern == "" {
			idEvaluator := &patternEvaluator{scope: p.scope}
			_ = ast.Walk(idEvaluator, v.Symbol.Binding.(*ast.PatternFragment))
			idPattern = idEvaluator.pattern
		}
		p.pattern += idPattern
		return p, v
	}
	return p, n
}

func (p *patternEvaluator) VisitAfter(n ast.Node) ast.Node {
	return n
}
