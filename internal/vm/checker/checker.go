// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package checker

import (
	"fmt"
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

const (
	kDefaultMaxRegexpLength   = 1024
	kDefaultMaxRecursionDepth = 100
)

// checker holds data for a semantic checker
type checker struct {
	scope *symbol.Scope // the current scope

	decoScopes []*symbol.Scope // A stack of scopes used for resolving symbols in decorated nodes

	errors errors.ErrorList

	depth             int
	tooDeep           bool
	maxRecursionDepth int
	maxRegexLength    int
}

// Check performs a semantic check of the astNode, and returns a potentially
// modified astNode and either a list of errors found, or nil if the program is
// semantically valid.  At the completion of Check, the symbol table and type
// annotation are also complete.
func Check(node ast.Node, maxRegexpLength int, maxRecursionDepth int) (ast.Node, error) {
	// set defaults
	if maxRegexpLength == 0 {
		maxRegexpLength = kDefaultMaxRegexpLength
	}
	if maxRecursionDepth == 0 {
		maxRecursionDepth = kDefaultMaxRecursionDepth
	}

	c := &checker{maxRegexLength: maxRegexpLength, maxRecursionDepth: maxRecursionDepth}
	node = ast.Walk(c, node)
	if len(c.errors) > 0 {
		return node, c.errors
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

	case *ast.IdTerm:
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
		id, ok := n.Id.(*ast.IdTerm)
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
			c.errors.Add(sym.Pos, fmt.Sprintf("Declaration of %s `%s' here is never used.", sym.Kind, sym.Name))
		}
	}
}

// VisitAfter performs the type annotation and checking, once the child nodes of
// expressions have been annotated and checked.
func (c *checker) VisitAfter(node ast.Node) ast.Node {
	if c.tooDeep {
		return node
	}
	defer func() { c.depth-- }()

	switch n := node.(type) {
	case *ast.StmtList:
		c.checkSymbolUsage()
		// Pop the scope
		c.scope = n.Scope.Parent
		return n

	case *ast.CondStmt:
		switch n.Cond.(type) {
		case *ast.BinaryExpr, *ast.OtherwiseStmt, *ast.UnaryExpr:
			// OK as conditions
		case *ast.PatternExpr:
			// The parser will always put a UnaryExpr here for a pattern, we will get a PatternExpr if the Cond was in fact an IdTerm rewritten by IndexedExpr below.
			n.Cond = &ast.UnaryExpr{Expr: n.Cond, Op: parser.MATCH}
		default:
			c.errors.Add(n.Cond.Pos(), fmt.Sprintf("Can't interpret %s as a boolean expression here.\n\tTry using comparison operators to make the condition explicit.", n.Cond.Type()))
		}
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
		// The last element in this list will be the empty stack created by the
		// DecoDecl on the way in.  If there's no last element, then we can't
		// have entered a DecoDecl yet.
		last := len(c.decoScopes) - 1
		if last < 0 {
			c.errors.Add(n.Pos(), fmt.Sprintf("Can't use `next' outside of a decorator."))
			return n
		}
		decoScope := c.decoScopes[last]
		if len(decoScope.Symbols) > 0 {
			c.errors.Add(n.Pos(), fmt.Sprintf("Can't use `next' statement twice in a decorator."))
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
				// Commented because these type mismatch errors appear to be unhelpful.
				//c.errors.Add(n.Pos(), err.Error())
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

			if n.Op == parser.DIV || n.Op == parser.MOD {
				if i, ok := n.Rhs.(*ast.IntLit); ok {
					if i.I == 0 {
						c.errors.Add(n.Pos(), "Can't divide by zero.")
						n.SetType(types.Error)
						return n
					}
				}
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
			astType := types.Function(lT, rT, rType)
			t := types.LeastUpperBound(lT, rT)
			if types.IsErrorType(t) {
				c.errors.Add(n.Pos(), fmt.Sprintf("Can't compare LHS of type %s with RHS of type %s.", lT, rT))
				n.SetType(t)
				return n
			}
			exprType := types.Function(t, t, types.Bool)
			err := types.Unify(exprType, astType)
			if err != nil {
				// Commented because these type mismatch errors appear to be unhelpful.
				//c.errors.Add(n.Pos(), err.Error())
				n.SetType(types.Error)
				return n
			}
			// Promote types if the ast types are not the same as the expression type.
			if !types.Equals(t, lT) {
				conv := &ast.ConvExpr{N: n.Lhs}
				conv.SetType(t)
				n.Lhs = conv
				glog.V(2).Infof("Emitting convnode %#v on %#v", conv, n)
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
				// Commented because these type mismatch errors appear to be unhelpful.
				//c.errors.Add(n.Pos(), err.Error())
				n.SetType(types.Error)
				return n
			}
			switch v := n.Lhs.(type) {
			case *ast.IdTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.Lhs.(*ast.IdTerm).Lvalue = true
			default:
				glog.V(2).Infof("The lhs is a %T %v", n.Lhs, n.Lhs)
				c.errors.Add(n.Lhs.Pos(), "Can't assign to this expression on the left.")
				n.SetType(types.Error)
				return n
			}

		case parser.MATCH, parser.NOT_MATCH:
			rType = types.Bool
			exprType := types.Function(types.NewVariable(), types.Pattern, rType)
			astType := types.Function(lT, rT, types.NewVariable())
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("Parameter to %s has a %s.", parser.Kind(n.Op), err))
				n.SetType(types.Error)
				return n
			}
			if !types.Equals(rT, types.Pattern) {
				n.Rhs = ast.Walk(c, &ast.PatternExpr{Expr: n.Rhs})
			}

		default:
			c.errors.Add(n.Pos(), fmt.Sprintf("Unexpected operator %s (%v) in node %#v", parser.Kind(n.Op), n.Op, n))
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
				// Commented because these type mismatch errors appear to be unhelpful.
				//	c.errors.Add(n.Pos(), fmt.Sprintf("type mismatch: %s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)
		case parser.INC, parser.DEC:
			// First check what sort of expression it is
			switch v := n.Expr.(type) {
			case *ast.IdTerm:
				v.Lvalue = true
			case *ast.IndexedExpr:
				v.Lhs.(*ast.IdTerm).Lvalue = true
			default:
				glog.V(2).Infof("the expr is a %T %v", n.Expr, n.Expr)
				c.errors.Add(n.Expr.Pos(), "Expecting a variable here.")
				n.SetType(types.Error)
				return n
			}
			rType := types.Int
			err := types.Unify(rType, t)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("%s", err))
				n.SetType(types.Error)
				return n
			}
			if !types.Equals(t, types.Int) {
				c.errors.Add(n.Expr.Pos(), fmt.Sprintf("Expecting an Int for %s, not %v.", parser.Kind(n.Op), t))
				n.SetType(types.Error)
				return n
			}
			glog.V(2).Infof("Return type is %v", rType)
			n.SetType(rType)

		case parser.MATCH:
			// Implicit match expressions, an expression of type Pattern returning Bool
			rType := types.Bool
			// recall the exprType is the language expectation
			exprType := types.Function(rType, types.Pattern)
			// and astType is the one we've been given
			astType := types.Function(types.NewVariable(), t)
			err := types.Unify(exprType, astType)
			if err != nil {
				c.errors.Add(n.Pos(), fmt.Sprintf("%s", err))
				n.SetType(types.Error)
				return n
			}
			n.SetType(rType)

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

		// prune this node to n.Lhs if Index is nil.  Leave 0 length exprlist as that's a type error.
		exprList, ok := n.Index.(*ast.ExprList)
		if !ok {
			return n.Lhs
		}

		argTypes := []types.Type{}
		for _, arg := range exprList.Children {
			if types.IsErrorType(arg.Type()) {
				n.SetType(types.Error)
				return n
			}
			argTypes = append(argTypes, arg.Type())
		}

		switch v := n.Lhs.(type) {
		case *ast.IdTerm:
			if v.Symbol == nil {
				// undefined, already caught
				n.SetType(types.Error)
				return n
			}

			if v.Type() == types.Pattern {
				// We now have enough information to tell that something the
				// parser thought was an IdTerm is really a pattern constant.
				// Let's now rewrite the AST correctly.
				// TODO: Haven't checked that this IndexedExpr has no args.
				return ast.Walk(c, &ast.PatternExpr{Expr: v})
			}

			if t, ok := v.Type().(*types.Operator); ok && types.IsDimension(t) {
				glog.V(1).Infof("Our idNode is a dimension type")
				// TODO: should this call n.SetType like below?
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

		switch n.Name {
		case "strptime":
			if !types.Equals(fn.Args[1], types.String) {
				c.errors.Add(n.Args.(*ast.ExprList).Children[1].Pos(), fmt.Sprintf("Expecting a format string for argument 2 of strptime(), not %v.", fn.Args[1]))
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
				timeStr := strings.Replace(strings.Replace(f.Text, "_", "", -1), "Z", "+", -1)
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

		case "tolower":
			if !types.Equals(fn.Args[0], types.String) {
				c.errors.Add(n.Args.(*ast.ExprList).Children[0].Pos(), fmt.Sprintf("Expecting a String for argument 1 of tolower(), not %v.", fn.Args[0]))
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
				c.errors.Add(n.Pos(), fmt.Sprintf("Cannot delete this.\n\tTry deleting an index from this dimensioned metric."))
				return n
			}
			ix.Lhs.(*ast.IdTerm).Lvalue = true
			return n
		}
		c.errors.Add(n.Pos(), fmt.Sprintf("Cannot delete this.\n\tTry deleting from a dimensioned metric with this as an index."))
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
	case *ast.IdTerm:
		// Already looked up sym, if still nil undefined.
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
