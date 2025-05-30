// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package compiler_test

import (
	"strings"
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/compiler"
	"github.com/jaqx0r/mtail/internal/testutil"
)

func makeCompiler(t *testing.T) *compiler.Compiler {
	t.Helper()
	c, err := compiler.New(compiler.EmitAst(), compiler.EmitAstTypes())
	testutil.FatalIfErr(t, err)
	return c
}

func TestCompileParserError(t *testing.T) {
	c := makeCompiler(t)
	r := strings.NewReader("bad program")
	_, err := c.Compile("test", r)
	if err == nil {
		t.Errorf("expected error, got nil")
	}
}

func TestCompileCheckerError(t *testing.T) {
	c := makeCompiler(t)
	r := strings.NewReader(`// {
i++
}`)
	_, err := c.Compile("test", r)
	if err == nil {
		t.Error("expected error, got nil")
	}
}

func TestCompileCodegen(t *testing.T) {
	c := makeCompiler(t)
	r := strings.NewReader(`counter i
// {
  i++
}`)
	_, err := c.Compile("test", r)
	if err != nil {
		t.Error(err)
	}
}
