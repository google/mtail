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
				switch n.Op {
				case parser.PLUS:
					lhs.I += rhs.I
				case parser.MINUS:
					lhs.I -= rhs.I
				case parser.MUL:
					lhs.I *= rhs.I
				case parser.DIV:
					lhs.I /= rhs.I
				case parser.MOD:
					lhs.I %= rhs.I
				case parser.POW:
					lhs.I = int64(math.Pow(float64(lhs.I), float64(rhs.I)))
				default:
					return node
				}
				lhs.P = *ast.MergePosition(&(lhs.P), &(rhs.P))
				return lhs
			case *ast.FloatLit:
				switch n.Op {
				case parser.PLUS:
					rhs.F += float64(lhs.I)
				case parser.MINUS:
					rhs.F = float64(lhs.I) - rhs.F
				case parser.MUL:
					rhs.F *= float64(lhs.I)
				case parser.DIV:
					rhs.F = float64(lhs.I) / rhs.F
				case parser.MOD:
					rhs.F = math.Mod(float64(lhs.I), rhs.F)
				case parser.POW:
					rhs.F = math.Pow(float64(lhs.I), rhs.F)
				default:
					return node
				}
				rhs.P = *ast.MergePosition(&(lhs.P), &(rhs.P))
				return rhs
			default:
				return node
			}
		case *ast.FloatLit:
			switch rhs := n.Rhs.(type) {
			case *ast.IntLit:
				switch n.Op {
				case parser.PLUS:
					lhs.F += float64(rhs.I)
				case parser.MINUS:
					lhs.F -= float64(rhs.I)
				case parser.MUL:
					lhs.F *= float64(rhs.I)
				case parser.DIV:
					lhs.F /= float64(rhs.I)
				case parser.MOD:
					lhs.F = math.Mod(lhs.F, float64(rhs.I))
				case parser.POW:
					lhs.F = math.Pow(lhs.F, float64(rhs.I))
				default:
					return node
				}
				lhs.P = *ast.MergePosition(&(lhs.P), &(rhs.P))
				return lhs
			case *ast.FloatLit:
				switch n.Op {
				case parser.PLUS:
					lhs.F += rhs.F
				case parser.MINUS:
					lhs.F -= rhs.F
				case parser.MUL:
					lhs.F *= rhs.F
				case parser.DIV:
					lhs.F /= rhs.F
				case parser.MOD:
					lhs.F = math.Mod(lhs.F, rhs.F)
				case parser.POW:
					lhs.F = math.Pow(lhs.F, rhs.F)
				default:
					return node
				}
				lhs.P = *ast.MergePosition(&(lhs.P), &(rhs.P))
				return lhs
			default:
				return node
			}
		default:
			return node
		}
	}
	return node
}
