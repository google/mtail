// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package opt_test

import (
	"testing"

	"github.com/google/mtail/internal/runtime/compiler/ast"
	"github.com/google/mtail/internal/runtime/compiler/opt"
	"github.com/google/mtail/internal/runtime/compiler/parser"
	"github.com/google/mtail/internal/testutil"
)

var optimiserTests = []struct {
	name string
	ast  ast.Node
	want ast.Node
}{
	{
		"int add",
		&ast.BinaryExpr{
			Lhs: &ast.IntLit{I: 1},
			Rhs: &ast.IntLit{I: 2},
			Op:  parser.PLUS,
		},
		&ast.IntLit{I: 3},
	},
	{
		"float mul",
		&ast.BinaryExpr{
			Lhs: &ast.FloatLit{F: 2},
			Rhs: &ast.FloatLit{F: 3},
			Op:  parser.MUL,
		},
		&ast.FloatLit{F: 6},
	},
	{
		"int float pow",
		&ast.BinaryExpr{
			Lhs: &ast.IntLit{I: 2},
			Rhs: &ast.FloatLit{F: 3},
			Op:  parser.POW,
		},
		&ast.FloatLit{F: 8},
	},
	{
		"float int mod",
		&ast.BinaryExpr{
			Lhs: &ast.FloatLit{F: 3},
			Rhs: &ast.IntLit{I: 2},
			Op:  parser.MOD,
		},
		&ast.FloatLit{F: 1},
	},
	{
		"nested ops",
		&ast.BinaryExpr{
			Lhs: &ast.BinaryExpr{
				Lhs: &ast.IntLit{I: 2},
				Rhs: &ast.IntLit{I: 4},
				Op:  parser.POW,
			},
			Rhs: &ast.IntLit{I: 1},
			Op:  parser.MINUS,
		},
		&ast.IntLit{I: 15},
	},
}

func TestOptimiser(t *testing.T) {
	for _, tc := range optimiserTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			got, err := opt.Optimise(tc.ast)
			testutil.FatalIfErr(t, err)
			testutil.ExpectNoDiff(t, tc.want, got)
		})
	}
}
