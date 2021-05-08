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

func TestOptimiser(t *testing.T) {
	n := &ast.BinaryExpr{
		Lhs: &ast.IntLit{I: 1},
		Rhs: &ast.IntLit{I: 2},
		Op:  parser.PLUS,
	}
	want := &ast.IntLit{I: 3}
	got, err := opt.Optimise(n)
	testutil.FatalIfErr(t, err)
	testutil.ExpectNoDiff(t, want, got)
}
