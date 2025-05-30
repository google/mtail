// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package types

import (
	"errors"
	"fmt"
	"testing"

	"github.com/jaqx0r/mtail/internal/testutil"
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
	// Unification of an Alternate with a concrete operator should be concrete.
	{
		Alternate(Bool, Int, Float), Bool,
		Bool,
	},
	{
		Pattern, Alternate(Bool, Pattern),
		Pattern,
	},
	// Unification of an Alternate with an Alternate operator should be a reduced Alternate
	{
		Alternate(Bool, Int, Float), Alternate(Bool, Int),
		Alternate(Bool, Int),
	},
	// Decompose an alternate to an operator.
	{
		Alternate(Pattern), Alternate(Bool, Pattern),
		Pattern,
	},
	{
		&Variable{}, Alternate(Int, Float),
		Alternate(Int, Float),
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
	// Patterns and Ints can only be bool.
	{
		Pattern, Int,
		Bool,
	},
	// Undef secedes to other
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
	// TypeError supercedes other.
	{
		Pattern, &TypeError{},
		&TypeError{},
	},
	{
		&TypeError{}, Float,
		&TypeError{},
	},
	// Numeric seceds to the concrete type
	{
		Numeric, Int,
		Int,
	},
	{
		Int, Numeric,
		Int,
	},
	{
		Numeric, Float,
		Float,
	},
	{
		Float, Numeric,
		Float,
	},
}

func TestTypeUnification(t *testing.T) {
	for _, tc := range typeUnificationTests {
		tc := tc
		t.Run(fmt.Sprintf("%s %s", tc.a, tc.b), func(t *testing.T) {
			tU := Unify(tc.a, tc.b)
			/* Type Errors never equal. */
			if IsTypeError(tc.expected) && IsTypeError(tU) {
				return
			}
			if !Equals(tc.expected, tU) {
				t.Errorf("want %q, got %q", tc.expected, tU)
			}
		})
	}
}

var groupOnlyMatchesTests = []struct {
	pattern  string
	check    string
	expected bool
}{
	{
		`\d+`,
		"0123456789",
		true,
	},
	{
		`[0123456789]`,
		"0123456789",
		true,
	},
	{
		`(0|1|2|3|4|5|6|7|8|9)`,
		"0123456789",
		true,
	},
	{
		`(\+|-)?\d+(\.\d+)?`,
		"0123456789",
		false,
	},
	{
		`(\d+\.\d+)`,
		"0123456789.eE+-",
		true,
	},
	{
		`(\+|-)?\d+(\.\d+)?`,
		"0123456789.eE+-",
		true,
	},
	{
		`(?P<offset>-?\d+\.\d+)`,
		"0123456789.eE+-",
		true,
	},
	{
		`(?P<x>-)`,
		"+-",
		true,
	},
	{
		`(?P<x>-)`,
		"+-0123456789",
		true,
	},
	{
		`\-`,
		"+-",
		true,
	},
	{
		`\-`,
		"+-0123456789",
		true,
	},
	{
		`\-|[0-9]`,
		"+-",
		false,
	},
}

func TestGroupOnlyMatches(t *testing.T) {
	for _, tc := range groupOnlyMatchesTests {
		r, err := ParseRegexp(tc.pattern)
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
	{
		`\d+`,
		Int,
	},
	{
		`-?\d+`,
		Int,
	},
	{
		`[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?`,
		Float,
	},
	{
		`-?\d+\.\d+`,
		Float,
	},
	{
		`(\d+\.\d+)`,
		Float,
	},
	{
		`\d+\.\d+\.\d+\.\d+`,
		String,
	},
	{
		`-`,
		String,
	},
	{
		`\-`,
		String,
	},
	// A single - is not an Int, so the whole class cannot be Int.
	{
		`[-0-9]`,
		String,
	},
	// Fun fact! This test gets simplified into `[\-0-9]` because the character
	// class is also an alternation.
	{
		`-|[0-9]`,
		String,
	},
	{
		`\d+\.\d+|\-`,
		String,
	},
	{
		`\-|\d+\.\d+`,
		String,
	},
}

func TestInferCaprefType(t *testing.T) {
	for _, tc := range inferCaprefTypeTests {
		tc := tc
		t.Run(tc.pattern, func(t *testing.T) {
			re, err := ParseRegexp(`(` + tc.pattern + `)`)
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

	var e *TypeError

	t1 := NewVariable()
	t2 := NewVariable()
	ty := Unify(t1, t2)
	if AsTypeError(ty, &e) {
		t.Fatal(e)
	}
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
	ty = Unify(Int, t3)
	if AsTypeError(ty, &e) {
		t.Fatal(e)
	}
	if !Equals(t3, Int) {
		t.Error("unified variable and const not same")
	}

	typeErr := &TypeError{}
	if Equals(typeErr, typeErr) {
		t.Error("error type equals itself")
	}
}

func TestAsTypeError(t *testing.T) {
	e := &TypeError{ErrTypeMismatch, Int, Bool}

	var e1 *TypeError
	if !AsTypeError(e, &e1) {
		t.Errorf("want type error, got: %#v", e1)
	}
	if !errors.Is(e1.error, ErrTypeMismatch) {
		t.Errorf("want ErrTypeMismatch, got: %#v", e1.error)
	}
	if e.expected != e1.expected || e.received != e1.received {
		t.Errorf("want %#v, got: %#v", e.expected, e1.expected)
		t.Errorf("want %#v, got: %#v", e.received, e1.received)
	}
}
