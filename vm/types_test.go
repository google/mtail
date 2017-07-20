// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"regexp/syntax"
	"testing"

	"github.com/go-test/deep"
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
	// Any type should unify to None with None.  You might call it the zero function.
	{
		None, Float,
		None,
	},
	{
		None, Int,
		None,
	},
	{
		Int, None,
		None,
	},
	{
		String, None,
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
		&TypeVariable{Id: 0}, &TypeVariable{Id: 0},
		&TypeVariable{Id: 0},
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
		t.Run(fmt.Sprintf("%s %s", tc.a, tc.b), func(t *testing.T) {
			t.Parallel()
			result := Unify(tc.a, tc.b)
			if diff := deep.Equal(tc.expected, result); len(diff) > 0 {
				t.Error(diff)
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
