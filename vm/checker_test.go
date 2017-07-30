// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	"github.com/go-test/deep"
)

var checkerInvalidPrograms = []struct {
	name    string
	program string
	errors  []string
}{
	{"undefined named capture group",
		"/blurgh/ { $undef++\n }\n",
		[]string{"undefined named capture group:1:12-17: Capture group `$undef' was not defined by a regular expression visible to this scope.\n\tTry using `(?P<undef>...)' to name the capture group."}},

	{"out of bounds capref",
		"/(blyurg)/ { $2++ \n}\n",
		[]string{"out of bounds capref:1:14-15: Capture group `$2' was not defined by a regular expression " +
			"visible to this scope.\n\tCheck that there are at least 2 pairs of parentheses."},
	},

	{"undefined decorator",
		"@foo {}\n",
		[]string{"undefined decorator:1:1-4: Decorator `foo' not defined.\n\tTry adding a definition `def foo {}' earlier in the program."}},

	{"undefined identifier",
		"// { x++ \n}\n",
		[]string{"undefined identifier:1:6: Identifier `x' not declared.\n\tTry adding `counter x' to the top of the program."},
	},

	{"invalid regex",
		"/foo(/ {}\n",
		[]string{"invalid regex:1:1-6: error parsing regexp: missing closing ): `foo(`"}},

	{"invalid regex 2",
		"/blurg(?P<x.)/ {}\n",
		[]string{"invalid regex 2:1:1-14: error parsing regexp: invalid named capture: `(?P<x.)`"}},

	{"invalid regex 3",
		"/blurg(?P<x>[[:alph:]])/ {}\n",
		[]string{"invalid regex 3:1:1-24: error parsing regexp: invalid character class range: `[:alph:]`"}},

	{"duplicate declaration",
		"counter foo\ncounter foo\n",
		[]string{"duplicate declaration:2:9-11: Redeclaration of metric `foo' previously declared at duplicate declaration:1:9-11"}},
}

func TestCheckInvalidPrograms(t *testing.T) {
	for _, tc := range checkerInvalidPrograms {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := Parse(tc.name, strings.NewReader(tc.program))
			if err != nil {
				t.Fatal(err)
			}
			err = Check(ast)
			if err == nil {
				t.Fatalf("check error: %s", err)
			}

			diff := deep.Equal(
				strings.Join(tc.errors, "\n"),        // want
				strings.TrimRight(err.Error(), "\n")) // got
			if diff != nil {
				t.Error(diff)
			}
		})
	}
}

var checkerValidPrograms = []struct {
	name    string
	program string
}{
	{"capture group",
		`counter foo
/(.*)/ {
  foo += $1
}
`,
	},
	{"shadowed positionals",
		`counter foo
/(.*)/ {
  foo += $1
  /bar(\d+)/ {
   foo += $1
  }
}
`},
	{"sibling positionals",
		`counter foo
/(.*)/ {
  foo += $1
}
/bar(\d+)/ {
   foo += $1
}
`},
}

func TestCheckValidPrograms(t *testing.T) {
	for _, tc := range checkerValidPrograms {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := Parse(tc.name, strings.NewReader(tc.program))
			if err != nil {
				t.Fatal(err)
			}
			err = Check(ast)
			if err != nil {
				t.Errorf("check failed: %s", err)
			}
		})
	}
}

var checkerTypeExpressionTests = []struct {
	name     string
	expr     astNode
	expected Type
}{
	{"Int + Int -> Float",
		&binaryExprNode{lhs: &intConstNode{position{}, 1},
			rhs: &intConstNode{position{}, 1},
			op:  PLUS},
		Int,
	},

	{"Int + Float -> Float",
		&binaryExprNode{lhs: &intConstNode{position{}, 1},
			rhs: &floatConstNode{position{}, 1.0},
			op:  PLUS},
		Float,
	},
	{"⍺ + Float -> Float",
		&binaryExprNode{lhs: &idNode{pos: position{}, sym: &Symbol{Name: "i", Kind: VarSymbol, Type: NewTypeVariable()}},
			rhs: &caprefNode{pos: position{}, sym: &Symbol{Kind: CaprefSymbol, Type: Float}},
			op:  ASSIGN},
		Float,
	},
}

func TestCheckTypeExpressions(t *testing.T) {
	defaultCompareUnexportedFields := deep.CompareUnexportedFields
	deep.CompareUnexportedFields = true
	defer func() { deep.CompareUnexportedFields = defaultCompareUnexportedFields }()

	for _, tc := range checkerTypeExpressionTests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			err := Check(tc.expr)
			if err != nil {
				t.Fatalf("check error: %s", err)
			}

			diff := deep.Equal(tc.expected, tc.expr.Type())
			if len(diff) > 0 {
				t.Error(diff)
			}
		})
	}
}
