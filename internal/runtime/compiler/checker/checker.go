// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package checker

import (
	goerrors "errors"
	"fmt"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/errors"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/symbol"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/types"
)

const (
	defaultMaxRegexpLength   = 1024
	defaultMaxRecursionDepth = 100
)

// checker holds data for a semantic checker.
type checker struct {
	scope *symbol.Scope // the current scope

	decoScopes []*symbol.Scope // A stack of scopes used for resolving symbols in decorated nodes

	errors errors.ErrorList

	depth             int
	tooDeep           bool
	maxRecursionDepth int
	maxRegexLength    int
	noRegexSymbols    bool
}

// Check performs a semantic check of the astNode, and returns a potentially
// modified astNode and either a list of errors found, or nil if the program is
// semantically valid.  At the completion of Check, the symbol table and type
// annotation are also complete.
func Check(node ast.Node, maxRegexpLength int, maxRecursionDepth int) (ast.Node, error) {
	// set defaults
	if maxRegexpLength == 0 {
		maxRegexpLength = defaultMaxRegexpLength
	}
	if maxRecursionDepth == 0 {
		maxRecursionDepth = defaultMaxRecursionDepth
	}

	c := &checker{maxRegexLength: maxRegexpLength, maxRecursionDepth: maxRecursionDepth}
	node = ast.Walk(c, node)
	if len(c.errors) > 0 {
		return node, &c.errors
	}
	return node, nil
}

// VisitBefore performs most of the symbol table construction, so that symbols
// are guaranteed to exist before their use.
func (c *checker) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	c.depth++
	if c.depth > c.maxRecursionDepth {
		if !c.tooDeep {
			c.errors.Add(node.Pos(), fmt.Sprintf("Expression exceeded maximum recursion depth of %d", c.maxRecursionDepth))
			c.tooDeep = true
		}
		c.depth--
		return nil, node
	}
	switch n := node.(type) {
	case *ast.StmtList:
		n.Scope = symbol.NewScope(c.scope)
		c.scope = n.Scope
		glog.V(2).Infof("Created new scope %v in stmtlist", n.Scope)
		return c, n

	case *ast.CondStmt:
		n.Scope = symbol.NewScope(c.scope)
		c.scope = n.Scope
		glog.V(2).Infof("Created new scope %v in condstmt", n.Scope)
		return c, n

	case *ast.BuiltinExpr:
		if n.Name == "subst" {
			c.noRegexSymbols = true
		}
		return c, n

	case *ast.CaprefTerm:
		if n.Symbol == nil {
			sym := c.scope.Lookup(n.Name, symbol.CaprefSymbol)
			if sym == nil {
				msg := fmt.Sprintf("Capture group `$%s' was not defined by a regular expression visible to this scope.", n.Name)
				if n.IsNamed {
					msg = fmt.Sprintf("%s\n\tTry using `(?P<%s>...)' to name the capture group.", msg, n.Name)
				} else {
					msg = fmt.Sprintf("%s\n\tCheck that there are at least %s pairs of parentheses.", msg, n.Name)
				}
				c.errors.Add(n.Pos(), msg)
				c.depth--
				return nil, n
			}
			glog.V(2).Infof("Found %q as %v in scope %v", n.Name, sym, c.scope)
			sym.Used = true
			n.Symbol = sym
		}
		return c, n

	case *ast.VarDecl:
		n.Symbol = symbol.NewSymbol(n.Name, symbol.VarSymbol, n.Pos())
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of metric `%s' previously declared at %s", n.Name, alt.Pos))
			c.depth--
			return nil, n
		}
		var rType types.Type
		switch n.Kind {
		case metrics.Counter, metrics.Gauge, metrics.Timer, metrics.Histogram:
			// TODO(jaq): This should be a numeric type, unless we want to
			// enforce more specific rules like "Counter can only be Int."
			rType = types.NewVariable()
		case metrics.Text:
			rType = types.String
		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("internal compiler error: unrecognised Kind %v for declNode %v", n.Kind, n))
			c.depth--
			return nil, n
		}
		if len(n.Buckets) > 0 && n.Kind != metrics.Histogram {
			c.errors.Add(n.Pos(), fmt.Sprintf("Can't specify buckets for non-histogram metric `%s'.", n.Name))
			c.depth--
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

	case *ast.IDTerm:
		if n.Symbol == nil {
			if sym := c.scope.Lookup(n.Name, symbol.VarSymbol); sym != nil {
				glog.V(2).Infof("found varsymbol sym %v", sym)
				sym.Used = true
				n.Symbol = sym
			} else if sym := c.scope.Lookup(n.Name, symbol.PatternSymbol); sym != nil {
				glog.V(2).Infof("Found patternsymbol Sym %v", sym)
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
				c.depth--
				return nil, n
			}
		}
		return c, n

	case *ast.DecoDecl:
		n.Symbol = symbol.NewSymbol(n.Name, symbol.DecoSymbol, n.Pos())
		n.Symbol.Binding = n
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redeclaration of decorator `@%s' previously declared at %s", n.Name, alt.Pos))
			c.depth--
			return nil, n
		}
		// Append a scope placeholder for the recursion into the block.  It has no parent, it'll be cloned when the decorator is instantiated.
		c.decoScopes = append(c.decoScopes, symbol.NewScope(nil))
		return c, n

	case *ast.DecoStmt:
		if sym := c.scope.Lookup(n.Name, symbol.DecoSymbol); sym != nil {
			if sym.Binding == nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: Decorator %q not bound to its definition.", n.Name))
				c.depth--
				return nil, n
			}
			sym.Used = true
			n.Decl = sym.Binding.(*ast.DecoDecl)
		} else {
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `@%s' is not defined.\n\tTry adding a definition `def %s {}' earlier in the program.", n.Name, n.Name))
			c.depth--
			return nil, n
		}
		// Create a new scope for the decorator instantiation.
		n.Scope = symbol.NewScope(c.scope)

		if n.Decl == nil {
			glog.V(2).Infof("No DecoDecl on DecoStmt: %v", n)
			c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: no declaration for decorator: %#v", n))
			c.depth--
			return nil, n
		}
		if n.Decl.Scope == nil {
			glog.V(2).Infof("No Scope on DecoDecl: %#v", n.Decl)
			c.errors.Add(n.Pos(), fmt.Sprintf("Decorator `@%s' is not completely defined yet.\n\tTry removing @%s from here.", n.Name, n.Name))
			c.depth--
			return nil, n
		}

		// Clone the DecoDecl scope zygote into this scope.
		n.Scope.CopyFrom(n.Decl.Scope)
		c.scope = n.Scope
		return c, n

	case *ast.PatternFragment:
		id, ok := n.ID.(*ast.IDTerm)
		if !ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("Internal error: no identifier attached to pattern fragment %#v", n))
			c.depth--
			return nil, n
		}
		n.Symbol = symbol.NewSymbol(id.Name, symbol.PatternSymbol, id.Pos())
		if alt := c.scope.Insert(n.Symbol); alt != nil {
			c.errors.Add(n.Pos(), fmt.Sprintf("Redefinition of pattern constant `%s' previously defined at %s", id.Name, alt.Pos))
			c.depth--
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

// checkSymbolTable emits errors if any eligible symbols in the current scope
// are not marked as used or have an invalid type.
func (c *checker) checkSymbolTable() {
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
				glog.Infof("capture group reference `%s' at %s appears to be unused", sym.Name, sym.Pos)
				continue
			}
			c.errors.Add(sym.Pos, fmt.Sprintf("Declaration of %s `%s' here is never used.", sym.Kind, sym.Name))
		}
	}
}

// VisitAfter performs the type annotation and checking, once the child nodes
// of expressions have been annotated and checked.  Within this function,
// gotType refers to the types inferred in the AST, and wantType is the type
// expected for this expression.  After unification, uType is the concrete type
// of the expression, and the visitor should set any node Types as appropriate.
//
// The notation for type inference used comes from the 2010 lecture notes for
// Stanford's CS413 class.
// https://web.stanford.edu/class/cs143/lectures/lecture09.pdf
func (c *checker) VisitAfter(node ast.Node) ast.Node {
	if c.tooDeep {
		return node
	}
	defer func() { c.depth-- }()

	switch n := node.(type) {
	case *ast.StmtList:
		c.checkSymbolTable()
		// Pop the scope
		c.scope = n.Scope.Parent
		return n

	case *ast.CondStmt:
		switch n.Cond.(type) {
		case *ast.BinaryExpr, *ast.OtherwiseStmt, *ast.UnaryExpr:
			// OK as conditions
		case *ast.PatternExpr:
			// If the parser saw an IDTerm with type Pattern, then we know it's really a pattern constant and need to wrap it in an unary match in this context.
			cond := &ast.UnaryExpr{Expr: n.Cond, Op: parser.MATCH}
			cond.SetType(types.Bool)
			n.Cond = cond
		default:
			c.errors.Add(n.Cond.Pos(), fmt.Sprintf("Can't interpret %s as a boolean expression here.\n\tTry using comparison operators to make the condition explicit.", n.Cond.Type()))
		}
		c.checkSymbolTable()
		// Pop the scope.
		c.scope = n.Scope.Parent
		return n

	case *ast.DecoStmt:
		// Don't check symbol usage here because the decorator is only partially defined.
		// Pop the scope.
		c.scope = n.Scope.Parent
		return n

	case *ast.NextStmt:
		// The last element in this list will be the empty stack created by the
		// DecoDecl on the way in.  If there's no last element, then we can't
		// have entered a DecoDecl yet.
		last := len(c.decoScopes) - 1
		if last < 0 {
			c.errors.Add(n.Pos(), "Can't use `next' outside of a decorator.")
			return n
		}
		decoScope := c.decoScopes[last]
		if len(decoScope.Symbols) > 0 {
			c.errors.Add(n.Pos(), "Can't use `next' statement twice in a decorator.")
			return n
		}
		// Merge the current scope into it.
		decoScope.CopyFrom(c.scope)
		return n

	case *ast.DecoDecl:
		// Pop the scope off the list, and insert it into this node.
		last := len(c.decoScopes) - 1
		decoScope := c.decoScopes[last]
		if len(decoScope.Symbols) == 0 {
			c.errors.Add(n.Pos(), fmt.Sprintf("No symbols found in decorator `@%s'.\n\tTry adding a `next' statement inside the `{}' block.", n.Name))
		}
		// Store the zygote from the scope stack on this declaration.
		n.Scope = decoScope
		c.decoScopes = c.decoScopes[:last]
		return n

	case *ast.BinaryExpr:
		lT := n.LHS.Type()
		if types.IsTypeError(lT) {
			n.SetType(lT)
			return n
		}
		rT := n.RHS.Type()
		if types.IsTypeError(rT) {
			n.SetType(rT)
			return n
		}
		var rType types.Type
		switch n.Op {
		case parser.DIV, parser.MOD, parser.MUL, parser.MINUS, parser.PLUS, parser.POW:
			// Arithmetic: e1 OP e2
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : lub(Tl, Tr)

			// First handle the Tl <= Tr and vice versa.
			rType = types.LeastUpperBound(lT, rT)
			var err *types.TypeError
			if types.AsTypeError(rType, &err) {
				// Change the type mismatch error to make more sense in this context.
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: can't apply %s to LHS of type %q with RHS of type %q.", parser.Kind(n.Op), lT, rT))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

			gotType := types.Function(lT, rT, rType)
			t := types.NewVariable()
			wantType := types.Function(t, t, t)
			uType := types.Unify(wantType, gotType)
			if types.AsTypeError(uType, &err) {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(err)
				return n
			}

			// Implicit type conversion for non-comparisons, promoting each
			// half to the return type of the op.
			if !types.Equals(rType, lT) {
				conv := &ast.ConvExpr{N: n.LHS}
				conv.SetType(rType)
				n.LHS = conv
			}
			if !types.Equals(rType, rT) {
				conv := &ast.ConvExpr{N: n.RHS}
				conv.SetType(rType)
				n.RHS = conv
			}

			if n.Op == parser.DIV || n.Op == parser.MOD {
				if i, ok := n.RHS.(*ast.IntLit); ok {
					if i.I == 0 {
						c.errors.Add(n.Pos(), "Can't divide by zero.")
						n.SetType(types.Error)
						return n
					}
				}
			}

		case parser.SHL, parser.SHR, parser.BITAND, parser.BITOR, parser.XOR, parser.NOT:
			// bitwise: e1 OP e2
			// O ⊢ e1 : Int, O ⊢ e2 : Int
			// ⇒ O ⊢ e : Int
			rType = types.Int
			wantType := types.Function(rType, rType, rType)
			gotType := types.Function(lT, rT, types.NewVariable())
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("Integer types expected for bitwise %s, got %s and %s", parser.Kind(n.Op), lT, rT))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

		case parser.AND, parser.OR:
			// If the parser saw an IDTerm with type Pattern, then we know it's really a pattern constant and need to wrap it in an unary match in this context.
			if v, ok := n.LHS.(*ast.PatternExpr); ok {
				match := &ast.UnaryExpr{Expr: v, Op: parser.MATCH}
				match.SetType(types.Bool)
				n.LHS = match
			}
			// Likewise for the RHS
			if v, ok := n.RHS.(*ast.PatternExpr); ok {
				match := &ast.UnaryExpr{Expr: v, Op: parser.MATCH}
				match.SetType(types.Bool)
				n.RHS = match
			}

			// logical: e1 OP e2
			// O ⊢ e1 : Bool, O ⊢ e2 : Bool
			// ⇒ O ⊢ e : Bool
			rType = types.Bool
			wantType := types.Function(rType, rType, rType)
			gotType := types.Function(lT, rT, types.NewVariable())
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("Boolean types expected for logical %s, got %s and %s", parser.Kind(n.Op), lT, rT))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

		case parser.LT, parser.GT, parser.LE, parser.GE, parser.EQ, parser.NE:
			// comparable, logical: e2 OP e2
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tl <= Tr , Tr <= Tl
			// ⇒ O ⊢ e : Bool

			// First handle the Tl <= Tr and vice versa.
			t := types.LeastUpperBound(lT, rT)
			var err *types.TypeError
			if types.AsTypeError(t, &err) {
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: can't apply %s to LHS of type %q with RHS of type %q.", parser.Kind(n.Op), lT, rT))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

			rType = types.Bool
			gotType := types.Function(lT, rT, rType)
			wantType := types.Function(t, t, types.Bool)
			uType := types.Unify(wantType, gotType)
			if types.AsTypeError(uType, &err) {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(err)
				return n
			}

			// Implicit type conversion: Promote types if the ast types are not
			// the same as the expression type.
			if !types.Equals(t, lT) {
				conv := &ast.ConvExpr{N: n.LHS}
				conv.SetType(t)
				n.LHS = conv
				glog.V(2).Infof("Emitting convnode %#v on %#v", conv, n)
			}
			if !types.Equals(t, rT) {
				conv := &ast.ConvExpr{N: n.RHS}
				conv.SetType(t)
				n.RHS = conv
				glog.V(2).Infof("Emitting convnode %+v", conv)
			}

		case parser.ASSIGN, parser.ADD_ASSIGN:
			// e1 = e2; e1 += e2
			// O ⊢ e1 : Tl, O ⊢ e2 : Tr
			// Tr <= Tl
			// ⇒ O ⊢ e : Tl
			rType = lT
			// TODO(jaq): the rT <= lT relationship is not correctly encoded here.
			t := types.LeastUpperBound(lT, rT)
			uType := types.Unify(rType, t)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(err)
				return n
			}

			// If the LHS is assignable, mark it as an lvalue, otherwise error.
			switch v := n.LHS.(type) {
			case *ast.IDTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.LHS.(*ast.IDTerm).Lvalue = true
			default:
				glog.V(2).Infof("The lhs is a %T %v", n.LHS, n.LHS)
				c.errors.Add(n.LHS.Pos(), "Can't assign to expression on left; expecting a variable here.")
				n.SetType(types.Error)
				return n
			}

		case parser.MATCH, parser.NOT_MATCH:
			// e1 =~ e2, e1 !~ e2
			// O ⊢ e1 : String , O ⊢ e2 : Pattern
			// ⇒ O ⊢ e : Bool
			// TODO(jaq): We're not correctly encoding this.
			rType = types.Bool
			wantType := types.Function(types.NewVariable(), types.Pattern, rType)
			gotType := types.Function(lT, rT, types.NewVariable())
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("Parameter to %s has a %s.", parser.Kind(n.Op), err))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

			// Implicit conversion of the RHS to a PatternExpr if not already.
			if !types.Equals(rT, types.Pattern) {
				n.RHS = ast.Walk(c, &ast.PatternExpr{Expr: n.RHS})
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator %s (%v) in node %#v", parser.Kind(n.Op), n.Op, n))
			n.SetType(types.InternalError)
			return n
		}
		n.SetType(rType)
		return n

	case *ast.UnaryExpr:
		if types.IsTypeError(n.Expr.Type()) {
			n.SetType(n.Expr.Type())
			return n
		}

		var rType types.Type
		switch n.Op {
		case parser.NOT:
			// !e1
			// O ⊢ e1 : Int
			// ⇒ O ⊢ e : Bool
			rType = types.Bool
			wantType := types.Function(types.Int, rType)
			gotType := types.Function(n.Expr.Type(), types.NewVariable())
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				c.errors.Add(n.Expr.Pos(), fmt.Sprintf("%s for `~' operator.", err))
				n.SetType(err)
				return n
			}

		case parser.INC, parser.DEC:
			// e1++ , e1--
			// O ⊢ e1 : Int
			// ⇒ O ⊢ e : Int

			// TODO we do this backwards versus ADD_ASSIGN above, why

			// If the expr is assignable, mark it as an lvalue, otherwise error.
			switch v := n.Expr.(type) {
			case *ast.IDTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.LHS.(*ast.IDTerm).Lvalue = true
			default:
				glog.V(2).Infof("the expr is a %T %v", n.Expr, n.Expr)
				c.errors.Add(n.Expr.Pos(), "Can't assign to expression; expecting a variable here.")
				n.SetType(types.Error)
				return n
			}

			rType = types.NewVariable()
			wantType := types.Function(types.Int, types.Int)
			gotType := types.Function(n.Expr.Type(), rType)
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				c.errors.Add(n.Pos(), err.Error())
				n.SetType(err)
				return n
			}

			uTypeOperator, ok := uType.(*types.Operator)
			if !ok {
				c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected type for Expr %v", uType))
				n.SetType(types.InternalError)
				return n
			}
			// After unification, the expr still has to be of Int type.
			if !types.OccursIn(types.Int, []types.Type{uTypeOperator.Args[0]}) {
				c.errors.Add(n.Expr.Pos(), fmt.Sprintf("type mismatch: expecting an Int for %s, not %v.", parser.Kind(n.Op), n.Expr.Type()))
				n.SetType(types.Error)
				return n
			}

		case parser.MATCH:
			// Implicit match expressions, an expression of type Pattern returning Bool
			// /e1/
			// O ⊢ e1 : Pattern
			// ⇒ O ⊢ e : Bool
			rType = types.Bool
			wantType := types.Function(types.Pattern, rType)
			gotType := types.Function(n.Expr.Type(), types.NewVariable())
			uType := types.Unify(wantType, gotType)
			var err *types.TypeError
			if types.AsTypeError(uType, &err) {
				if goerrors.Is(err, types.ErrTypeMismatch) {
					c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: Unary MATCH expects Pattern, received %s", n.Expr.Type()))
				} else {
					c.errors.Add(n.Pos(), err.Error())
				}
				n.SetType(err)
				return n
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("unknown unary op %s in expr %#v", parser.Kind(n.Op), n))
			n.SetType(types.InternalError)
			return n
		}
		n.SetType(rType)
		return n

	case *ast.ExprList:
		// (e1, e2, ...)
		// ⇒ O ⊢ e: e1⨯e1⨯...
		argTypes := []types.Type{}
		for _, arg := range n.Children {
			if types.IsTypeError(arg.Type()) {
				n.SetType(arg.Type())
				return n
			}
			argTypes = append(argTypes, arg.Type())
		}
		n.SetType(types.Dimension(argTypes...))
		return n

	case *ast.IndexedExpr:
		// e1[e2, e3, ..., en]
		// O ⊢ e1 : T1⨯T2⨯...Tn⨯Tr
		// O ⊢ e2,e3,...,en : T1,T2,...,Tn
		// ⇒ O ⊢ e : Tr

		// prune this node to n.LHS if Index is nil.  Leave 0 length exprlist as that's a type error.
		exprList, ok := n.Index.(*ast.ExprList)
		if !ok {
			return n.LHS
		}

		argTypes := []types.Type{}
		for _, arg := range exprList.Children {
			if types.IsTypeError(arg.Type()) {
				n.SetType(arg.Type())
				return n
			}
			argTypes = append(argTypes, arg.Type())
		}

		switch v := n.LHS.(type) {
		case *ast.IDTerm:
			if v.Symbol == nil {
				// undefined, already caught (where?)
				glog.V(2).Infof("undefined ID %v", v)
				n.SetType(types.Error)
				return n
			}

			if types.Equals(types.Pattern, v.Type()) {
				// We now have enough information to tell that something the
				// parser thought was an IDTerm is really a pattern constant,
				// so we can rewrite the AST here.  We can't yet wrap the
				// pattern expression with Unary Match because we don't know
				// the context yet, but see CondExpr and BinaryExpr's
				// logical-op.
				return ast.Walk(c, &ast.PatternExpr{Expr: v})
			}

			if !types.IsDimension(v.Type()) {
				if len(argTypes) > 0 {
					c.errors.Add(n.Pos(), "Index taken on unindexable expression")
					n.SetType(types.Error)
				} else {
					n.SetType(v.Type())
				}
				return n
			}

			// it's a Dimension, continue after switch

		default:
			c.errors.Add(n.Pos(), "Index taken on unindexable expression")
			n.SetType(types.Error)
			return n
		}

		rType := types.NewVariable()
		argTypes = append(argTypes, rType)
		// T1,T2,...,Tn
		gotType := types.Dimension(argTypes...)
		// Tr
		wantType, ok := n.LHS.Type().(*types.Operator)
		if !ok {
			c.errors.Add(n.Pos(), fmt.Sprintf("internal error: unexpected type on LHS %v", n.LHS.Type()))
			n.SetType(types.InternalError)
			return n
		}
		uType := types.Unify(wantType, gotType)
		var err *types.TypeError
		if types.AsTypeError(uType, &err) {
			switch {
			case len(wantType.Args) > len(gotType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Not enough keys for indexed expression: expecting %d, received %d", len(wantType.Args)-1, len(gotType.Args)-1))
				n.SetType(types.Error)
				return n
			case len(wantType.Args) < len(gotType.Args):
				c.errors.Add(n.Pos(), fmt.Sprintf("Too many keys for indexed expression: expecting %d, received %d.", len(wantType.Args)-1, len(gotType.Args)-1))
			default:
				c.errors.Add(n.Pos(), err.Error())
			}
			n.SetType(types.Error)
			return n
		}

		// Having typechecked the expression against the expected types, and
		// have detected mismatched keylengths, we have a well-formed
		// expression, so can now fold to just IDTerm if there's no ExprList.
		if len(exprList.Children) == 0 {
			return n.LHS
		}

		n.SetType(rType)
		return n

	case *ast.BuiltinExpr:
		// f(e1, e2, ..., en)
		// O ⊢ f : T1⨯T2⨯...Tn⨯Tr
		// O ⊢ e1,e2,...,en : T1,T2,...,Tn
		// ⇒ O ⊢ e : Tr
		// TODO: recall the syntax for subst a fresh type above
		argTypes := []types.Type{}
		if args, ok := n.Args.(*ast.ExprList); ok {
			for _, arg := range args.Children {
				argTypes = append(argTypes, arg.Type())
			}
		}
		rType := types.NewVariable()
		argTypes = append(argTypes, rType)

		gotType := types.Function(argTypes...)
		wantType := types.FreshType(types.Builtins[n.Name])
		uType := types.Unify(wantType, gotType)
		var err *types.TypeError
		if types.AsTypeError(uType, &err) {
			if goerrors.Is(err, types.ErrTypeMismatch) {
				c.errors.Add(n.Pos(), fmt.Sprintf("call to `%s': %s", n.Name, err))
			} else {
				c.errors.Add(n.Pos(), err.Error())
			}
			n.SetType(err)
			return n
		}
		n.SetType(rType)

		switch n.Name {
		case "strptime":
			if !types.Equals(gotType.Args[1], types.String) {
				c.errors.Add(n.Args.(*ast.ExprList).Children[1].Pos(), fmt.Sprintf("Expecting a format string for argument 2 of strptime(), not %v.", gotType.Args[1]))
				n.SetType(types.Error)
				return n
			}
			// Second argument to strptime is the format string.  If it is
			// defined at compile time, we can verify it can be use as a format
			// string by parsing itself.
			if f, ok := n.Args.(*ast.ExprList).Children[1].(*ast.StringLit); ok {
				// Layout strings can contain an underscore to indicate a digit
				// field if the layout field can contain two digits; but they
				// won't parse themselves.  Zulu Timezones in the layout need
				// to be converted to offset in the parsed time.
				timeStr := strings.ReplaceAll(strings.ReplaceAll(f.Text, "_", ""), "Z", "+")
				glog.V(2).Infof("time_str is %q", timeStr)
				_, err := time.Parse(f.Text, timeStr)
				if err != nil {
					glog.Infof("time.Parse(%q, %q) failed: %s", f.Text, timeStr, err)
					c.errors.Add(f.Pos(), fmt.Sprintf("invalid time format string %q\n\tRefer to the documentation at https://golang.org/pkg/time/#pkg-constants for advice.", f.Text))
					n.SetType(types.Error)
					return n
				}
			} else {
				c.errors.Add(n.Pos(), "Internal error: exprlist child is not string literal.")
				return n
			}

		case "subst":
			c.noRegexSymbols = false
			return n

		case "tolower":
			if !types.Equals(gotType.Args[0], types.String) {
				c.errors.Add(n.Args.(*ast.ExprList).Children[0].Pos(), fmt.Sprintf("Expecting a String for argument 1 of tolower(), not %v.", gotType.Args[0]))
				n.SetType(types.Error)
				return n
			}
		}
		return n

	case *ast.PatternExpr:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n = ast.Walk(pe, n).(*ast.PatternExpr)
		if pe.pattern.String() == "" {
			return n
		}
		n.Pattern = pe.pattern.String()
		c.checkRegex(n.Pattern, n)
		return n

	case *ast.PatternFragment:
		// Evaluate the expression.
		pe := &patternEvaluator{scope: c.scope, errors: &c.errors}
		n.Expr = ast.Walk(pe, n.Expr)
		if pe.pattern.String() == "" {
			return n
		}
		n.Pattern = pe.pattern.String()
		return n

	case *ast.DelStmt:
		if ix, ok := n.N.(*ast.IndexedExpr); ok {
			if len(ix.Index.(*ast.ExprList).Children) == 0 {
				c.errors.Add(n.N.Pos(), "Cannot delete this.\n\tTry deleting an index from this dimensioned metric.")
				return n
			}
			ix.LHS.(*ast.IDTerm).Lvalue = true
			return n
		}
		c.errors.Add(n.N.Pos(), "Cannot delete this.\n\tTry deleting from a dimensioned metric with this as an index.")
	}

	return node
}

// checkRegex is a helper method to compile and check a regular expression, and
// to generate its capture groups as symbols.
func (c *checker) checkRegex(pattern string, n ast.Node) {
	plen := len(pattern)
	if plen > c.maxRegexLength {
		c.errors.Add(n.Pos(), fmt.Sprintf("Exceeded maximum regular expression pattern length of %d bytes with %d.\n\tExcessively long patterns are likely to cause compilation and runtime performance problems.", c.maxRegexLength, plen))
		return
	}
	if reAst, err := types.ParseRegexp(pattern); err == nil {
		if c.noRegexSymbols {
			return
		}

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
			glog.V(2).Infof("Added capref %v to scope %v", sym, c.scope)
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
	pattern strings.Builder
}

func (p *patternEvaluator) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	switch v := n.(type) {
	case *ast.BinaryExpr:
		if v.Op != parser.PLUS {
			p.errors.Add(v.Pos(), fmt.Sprintf("internal error: Invalid operator in concatenation: %v", v))
			return nil, n
		}
		return p, v
	case *ast.PatternLit:
		p.pattern.WriteString(v.Pattern)
		return p, v
	case *ast.IDTerm:
		// Already looked up sym, if still nil then undefined.
		if v.Symbol == nil {
			return nil, n
		}
		pf, ok := v.Symbol.Binding.(*ast.PatternFragment)
		if !ok {
			p.errors.Add(v.Pos(), fmt.Sprintf("Can't append %s `%s' to this pattern.\n\tTry using a `const'-defined pattern fragment.", v.Symbol.Kind, v.Symbol.Name))
			return nil, n
		}
		if pf.Pattern == "" {
			p.errors.Add(v.Pos(), fmt.Sprintf("Can't evaluate pattern fragment `%s' here.\n\tTry defining it earlier in the program.", pf.Symbol.Name))
		}
		p.pattern.WriteString(pf.Pattern)
		return p, v
	case *ast.IntLit:
		p.pattern.WriteString(fmt.Sprintf("%d", v.I))
		return p, v
	case *ast.FloatLit:
		p.pattern.WriteString(fmt.Sprintf("%g", v.F))
		return p, v
	case *ast.StringLit:
		p.pattern.WriteString(v.Text)
		return p, v
	}
	return p, n
}

func (p *patternEvaluator) VisitAfter(n ast.Node) ast.Node {
	return n
}
