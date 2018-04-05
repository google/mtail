// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"testing"
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
		&TypeVariable{ID: 0}, &TypeVariable{ID: 0},
		&TypeVariable{ID: 0},
	},
	// The unification of any type operator with a type variable is the type operator
	{
		&TypeVariable{}, None,
		None,
	},
	{
		&TypeVariable{}, Float,
		Float,
	},
	{
		&TypeVariable{}, Int,
		Int,
	},
	{
		&TypeVariable{}, String,
		String,
	},
	{
		None, &TypeVariable{},
		None,
	},
	{
		Float, &TypeVariable{},
		Float,
	},
	{
		Int, &TypeVariable{},
		Int,
	},
	{
		String, &TypeVariable{},
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
}

func TestTypeUnification(t *testing.T) {
	for _, tc := range typeUnificationTests {
		tc := tc
		t.Run(fmt.Sprintf("%s %s", tc.a, tc.b), func(t *testing.T) {
			t.Parallel()
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
}

func TestGroupOnlyMatches(t *testing.T) {
	for _, tc := range groupOnlyMatchesTests {
		r, err := syntax.Parse(tc.pattern, syntax.Perl)
		if err != nil {
			t.Fatalf("syntax.Parse failed: %s", err)
		}
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
	{`\d+\.\d+\.\d+\.\d+`,
		String,
	},
}

func TestInferCaprefType(t *testing.T) {
	for _, tc := range inferCaprefTypeTests {
		tc := tc
		t.Run(tc.pattern, func(t *testing.T) {
			t.Parallel()
			re, err := syntax.Parse(`(`+tc.pattern+`)`, syntax.Perl)
			if err != nil {
				t.Fatal(err)
			}
			r := inferCaprefType(re, 1)
			if !Equals(tc.typ, r) {
				t.Errorf("Types don't match: %q infers %v, not %v", tc.pattern, r, tc.typ)
			}
		})
	}
}

func TestTypeEquals(t *testing.T) {
	if Equals(NewTypeVariable(), NewTypeVariable()) {
		t.Error("Type variables are not same")
	}

	t1 := NewTypeVariable()
	t2 := NewTypeVariable()
	err := Unify(t1, t2)
	if err != nil {
		t.Fatal(err)
	}
	if !Equals(t1, t2) {
		t.Errorf("Unified variables should be same: %v %v", t1, t2)
	}
	if !Equals(Int, Int) {
		t.Errorf("type constants not same")
	}

	t3 := NewTypeVariable()
	if Equals(t3, Int) {
		t.Error("ununified type const and var")
	}
	err = Unify(Int, t3)

	if err != nil {
		t.Fatal(err)
	}
	if !Equals(t3, Int) {
		t.Error("unified variable and const not same")
	}
}
