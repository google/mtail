// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package types

import (
	"fmt"
	"regexp/syntax"
	"testing"

	"github.com/google/mtail/internal/testutil"
)

var typeUnificationTests = []struct {
	a, b     Type
	expected Type
}{
	// The unification of None with None is still None.
	{
		None, None,
		None,
	},
	// The unification of a type T with itself is T.
	{
		String, String,
		String,
	},
	{
		Int, Int,
		Int,
	},
	{
		Float, Float,
		Float,
	},
	{
		&Variable{ID: 0}, &Variable{ID: 0},
		&Variable{ID: 0},
	},
	// The unification of any type operator with a type variable is the type operator
	{
		&Variable{}, None,
		None,
	},
	{
		&Variable{}, Float,
		Float,
	},
	{
		&Variable{}, Int,
		Int,
	},
	{
		&Variable{}, String,
		String,
	},
	{
		None, &Variable{},
		None,
	},
	{
		Float, &Variable{},
		Float,
	},
	{
		Int, &Variable{},
		Int,
	},
	{
		String, &Variable{},
		String,
	},
	// The lub of Int and Float is Float.
	{
		Int, Float,
		Float,
	},
	{
		Float, Int,
		Float,
	},
	// The lub of Int and String is String.
	{
		Int, String,
		String,
	},
	{
		String, Int,
		String,
	},
	// The lub of Float and String is String.
	{
		Float, String,
		String,
	},
	{
		String, Float,
		String,
	},
	// Implicitly, a Pattern by itself returns the value of its' match
	{
		Pattern, Bool,
		Bool,
	},
	{
		Bool, Pattern,
		Bool,
	},
	// lub of Bool and Int is an Int.
	{
		Bool, Int,
		Int,
	},
	{
		Int, Bool,
		Int,
	},
	// Strings can be Patterns.
	{
		Pattern, String,
		Pattern,
	},
	{
		String, Pattern,
		Pattern,
	},
	// Patterns and Ints can both be Bool.
	{
		Pattern, Int,
		Bool,
	},
	// Undef secedes to oether
	{
		Undef, Int,
		Int,
	},
	{
		String, Undef,
		String,
	},
	{
		Undef, Undef,
		Undef,
	},
}

func TestTypeUnification(t *testing.T) {
	for _, tc := range typeUnificationTests {
		tc := tc
		t.Run(fmt.Sprintf("%s %s", tc.a, tc.b), func(t *testing.T) {
			err := Unify(tc.a, tc.b)
			if err != nil {
				t.Errorf("%s", err)
			}
		})
	}
}

var groupOnlyMatchesTests = []struct {
	pattern  string
	check    string
	expected bool
}{
	{`\d+`,
		"0123456789",
		true,
	},
	{`[0123456789]`,
		"0123456789",
		true,
	},
	{`(0|1|2|3|4|5|6|7|8|9)`,
		"0123456789",
		true,
	},
	{`(\+|-)?\d+(\.\d+)?`,
		"0123456789",
		false,
	},
	{`(\d+\.\d+)`,
		"0123456789.eE+-",
		true,
	},
	{`(\+|-)?\d+(\.\d+)?`,
		"0123456789.eE+-",
		true,
	},
	{`(?P<offset>-?\d+\.\d+)`,
		"0123456789.eE+-",
		true,
	},
	{`(?P<x>-)`,
		"+-",
		true,
	},
	{`(?P<x>-)`,
		"+-0123456789",
		true,
	},
	{`\-`,
		"+-",
		true,
	},
	{`\-`,
		"+-0123456789",
		true,
	},
	{`\-|[0-9]`,
		"+-",
		false,
	},
}

func TestGroupOnlyMatches(t *testing.T) {
	for _, tc := range groupOnlyMatchesTests {
		r, err := syntax.Parse(tc.pattern, syntax.Perl)
		testutil.FatalIfErr(t, err)
		result := groupOnlyMatches(r, tc.check)
		if result != tc.expected {
			t.Errorf("Pattern %q didn't only match check %q: expected %+v, received %+v", tc.pattern, tc.check, tc.expected, result)
		}
	}
}

var inferCaprefTypeTests = []struct {
	pattern string
	typ     Type
}{
	{`\d+`,
		Int,
	},
	{`[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?`,
		Float,
	},
	{`-?\d+\.\d+`,
		Float,
	},
	{`(\d+\.\d+)`,
		Float,
	},
	{`\d+\.\d+\.\d+\.\d+`,
		String,
	},
	{`-`,
		String,
	},
	{`\-`,
		String,
	},
	{`\-|[0-9]`,
		String,
	},
	{`\d+\.\d+|\-`,
		String,
	},
	{`\-|\d+\.\d+`,
		String,
	},
}

func TestInferCaprefType(t *testing.T) {
	for _, tc := range inferCaprefTypeTests {
		tc := tc
		t.Run(tc.pattern, func(t *testing.T) {
			re, err := syntax.Parse(`(`+tc.pattern+`)`, syntax.Perl)
			testutil.FatalIfErr(t, err)
			r := InferCaprefType(re, 1)
			if !Equals(tc.typ, r) {
				t.Errorf("Types don't match: %q inferred %v, not %v", tc.pattern, r, tc.typ)
			}
		})
	}
}

func TestTypeEquals(t *testing.T) {
	if Equals(NewVariable(), NewVariable()) {
		t.Error("Type variables are not same")
	}

	t1 := NewVariable()
	t2 := NewVariable()
	err := Unify(t1, t2)
	testutil.FatalIfErr(t, err)
	if !Equals(t1, t2) {
		t.Errorf("Unified variables should be same: %v %v", t1, t2)
	}
	if !Equals(Int, Int) {
		t.Errorf("type constants not same")
	}

	t3 := NewVariable()
	if Equals(t3, Int) {
		t.Error("ununified type const and var")
	}
	err = Unify(Int, t3)
	testutil.FatalIfErr(t, err)
	if !Equals(t3, Int) {
		t.Error("unified variable and const not same")
	}
}
