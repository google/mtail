// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
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
	// The unification of Int and Float is Float.
	{
		Int, Float,
		Float,
	},
	{
		Float, Int,
		Float,
	},
	// The unification of Int and String is String.
	{
		Int, String,
		String,
	},
	{
		String, Int,
		String,
	},
	// The unification of Float and String is String.
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
	for i, tc := range typeUnificationTests {
		result := Unify(tc.a, tc.b)
		if diff := deep.Equal(result, tc.expected); len(diff) > 0 {
			t.Errorf("Result type not expected for %d: inputs %+v and %+v:\n%s", i, tc.a, tc.b, diff)
		}
	}
}
