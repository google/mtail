// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// package opt has a compiler pass for making optimisations on the AST.
package opt

import (
	"math"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/errors"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/types"
)

func Optimise(n ast.Node) (ast.Node, error) {
	o := &optimiser{}
	r := ast.Walk(o, n)
	if len(o.errors) > 0 {
		return r, &o.errors
	}
	return r, nil
}

type optimiser struct {
	errors errors.ErrorList
}

func (o *optimiser) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	return o, node
}

func (o *optimiser) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.BinaryExpr:
		switch lhs := n.LHS.(type) {
		case *ast.IntLit:
			switch rhs := n.RHS.(type) {
			case *ast.IntLit:
				r := &ast.IntLit{P: *position.Merge(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.I = lhs.I + rhs.I
				case parser.MINUS:
					r.I = lhs.I - rhs.I
				case parser.MUL:
					r.I = lhs.I * rhs.I
				case parser.DIV:
					if rhs.I == 0 {
						o.errors.Add(n.Pos(), "divide by zero")
						n.SetType(types.Error)
						return n
					}
					r.I = lhs.I / rhs.I
				case parser.MOD:
					if rhs.I == 0 {
						o.errors.Add(n.Pos(), "mod by zero")
						n.SetType(types.Error)
						return n
					}
					r.I = lhs.I % rhs.I
				case parser.POW:
					r.I = int64(math.Pow(float64(lhs.I), float64(rhs.I)))
				default:
					return node
				}
				return r
			case *ast.FloatLit:
				r := &ast.FloatLit{P: *position.Merge(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = float64(lhs.I) + rhs.F
				case parser.MINUS:
					r.F = float64(lhs.I) - rhs.F
				case parser.MUL:
					r.F = float64(lhs.I) * rhs.F
				case parser.DIV:
					if rhs.F == 0 {
						o.errors.Add(n.Pos(), "divide by zero")
						n.SetType(types.Error)
						return n
					}
					r.F = float64(lhs.I) / rhs.F
				case parser.MOD:
					if rhs.F == 0 {
						o.errors.Add(n.Pos(), "mod by zero")
						n.SetType(types.Error)
						return n
					}
					rhs.F = math.Mod(float64(lhs.I), rhs.F)
				case parser.POW:
					r.F = math.Pow(float64(lhs.I), rhs.F)
				default:
					return node
				}
				return r
			default:
				return node
			}
		case *ast.FloatLit:
			switch rhs := n.RHS.(type) {
			case *ast.IntLit:
				r := &ast.FloatLit{P: *position.Merge(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = lhs.F + float64(rhs.I)
				case parser.MINUS:
					r.F = lhs.F - float64(rhs.I)
				case parser.MUL:
					r.F = lhs.F * float64(rhs.I)
				case parser.DIV:
					if rhs.I == 0 {
						o.errors.Add(n.Pos(), "divide by zero")
						n.SetType(types.Error)
						return n
					}
					r.F = lhs.F / float64(rhs.I)
				case parser.MOD:
					if rhs.I == 0 {
						o.errors.Add(n.Pos(), "mod by zero")
						n.SetType(types.Error)
						return n
					}
					r.F = math.Mod(lhs.F, float64(rhs.I))
				case parser.POW:
					r.F = math.Pow(lhs.F, float64(rhs.I))
				default:
					return node
				}
				return r
			case *ast.FloatLit:
				r := &ast.FloatLit{P: *position.Merge(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = lhs.F + rhs.F
				case parser.MINUS:
					r.F = lhs.F - rhs.F
				case parser.MUL:
					r.F = lhs.F * rhs.F
				case parser.DIV:
					if rhs.F == 0 {
						o.errors.Add(n.Pos(), "divide by zero")
						n.SetType(types.Error)
						return n
					}
					r.F = lhs.F / rhs.F
				case parser.MOD:
					if rhs.F == 0 {
						o.errors.Add(n.Pos(), "mod by zero")
						n.SetType(types.Error)
						return n
					}
					r.F = math.Mod(lhs.F, rhs.F)
				case parser.POW:
					r.F = math.Pow(lhs.F, rhs.F)
				default:
					return node
				}
				return r
			default:
				return node
			}
		default:
			return node
		}
	default:
		return node
	}
}
