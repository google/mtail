// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// package opt has a compiler pass for making optimisations on the AST.
package opt

import "github.com/google/mtail/internal/runtime/compiler/ast"

func Optimise(ast ast.Node) (ast.Node, error) {
	return ast, nil
}
