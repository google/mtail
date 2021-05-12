// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// package opt has a compiler pass for making optimisations on the AST.
package opt

import (
	"math"

	"github.com/google/mtail/internal/runtime/compiler/ast"
	"github.com/google/mtail/internal/runtime/compiler/parser"
)

func Optimise(n ast.Node) (ast.Node, error) {
	o := &optimiser{}
	return ast.Walk(o, n), nil
}

type optimiser struct {
}

func (o *optimiser) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	return o, node
}

func (o *optimiser) VisitAfter(node ast.Node) ast.Node {
	switch n := node.(type) {
	case *ast.BinaryExpr:
		switch lhs := n.Lhs.(type) {
		case *ast.IntLit:
			switch rhs := n.Rhs.(type) {
			case *ast.IntLit:
				r := &ast.IntLit{P: *ast.MergePosition(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.I = lhs.I + rhs.I
				case parser.MINUS:
					r.I = lhs.I - rhs.I
				case parser.MUL:
					r.I = lhs.I * rhs.I
				case parser.DIV:
					r.I = lhs.I / rhs.I
				case parser.MOD:
					r.I = lhs.I % rhs.I
				case parser.POW:
					r.I = int64(math.Pow(float64(lhs.I), float64(rhs.I)))
				default:
					return node
				}
				return r
			case *ast.FloatLit:
				r := &ast.FloatLit{P: *ast.MergePosition(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = float64(lhs.I) + rhs.F
				case parser.MINUS:
					r.F = float64(lhs.I) - rhs.F
				case parser.MUL:
					r.F = float64(lhs.I) * rhs.F
				case parser.DIV:
					r.F = float64(lhs.I) / rhs.F
				case parser.MOD:
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
			switch rhs := n.Rhs.(type) {
			case *ast.IntLit:
				r := &ast.FloatLit{P: *ast.MergePosition(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = lhs.F + float64(rhs.I)
				case parser.MINUS:
					r.F = lhs.F - float64(rhs.I)
				case parser.MUL:
					r.F = lhs.F * float64(rhs.I)
				case parser.DIV:
					r.F = lhs.F / float64(rhs.I)
				case parser.MOD:
					r.F = math.Mod(lhs.F, float64(rhs.I))
				case parser.POW:
					r.F = math.Pow(lhs.F, float64(rhs.I))
				default:
					return node
				}
				return r
			case *ast.FloatLit:
				r := &ast.FloatLit{P: *ast.MergePosition(&(lhs.P), &(rhs.P))}
				switch n.Op {
				case parser.PLUS:
					r.F = lhs.F + rhs.F
				case parser.MINUS:
					r.F = lhs.F - rhs.F
				case parser.MUL:
					r.F = lhs.F * rhs.F
				case parser.DIV:
					r.F = lhs.F / rhs.F
				case parser.MOD:
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
	}
	return node
}
