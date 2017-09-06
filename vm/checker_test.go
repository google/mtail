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

	// 	{"indexedExpr parameter count",
	// 		`counter foo by a, b
	// counter bar by a, b
	// counter quux by a
	// /(\d+)/ {
	//   foo[$1]++
	//   bar[$1][0]++
	//   quux[$1][0]++
	// }
	// 	`,
	// 		[]string{"indexedExpr parameter count:5:3-8: index lookup: type mismatch: \"→ typeVar1 typeVar2 typeVar3\" != \"→ Int typeVar9\"",
	// 			"indexedExpr parameter count:7:3-12: Too many keys for metric"}},

	{"builtin parameter mismatch",
		`/(\d+)/ {
	  strptime()
	}
    /(\d+)/ {
	  timestamp()
	}
	`,
		[]string{"builtin parameter mismatch:2:13: call to `strptime': type mismatch; expected \"→ String None\" received \"→ typeVar5\""}},
}

func TestCheckInvalidPrograms(t *testing.T) {
	for _, tc := range checkerInvalidPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := Parse(tc.name, strings.NewReader(tc.program))
			if err != nil {
				t.Fatal(err)
			}
			err = Check(ast)
			if err == nil {
				t.Fatal("check didn't fail")
			}

			diff := deep.Equal(
				strings.Join(tc.errors, "\n"),        // want
				strings.TrimRight(err.Error(), "\n")) // got
			if diff != nil {
				t.Error(diff)
				s := Sexp{}
				s.emitTypes = true
				t.Log(s.Dump(ast))
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

	{"index expression",
		`counter foo by a, b
/(\d)/ {
  foo[1,$1] = 3
}`},

	{"odd indexes",
		`counter foo by a,b,c
/(\d) (\d)/ {
  foo[$1,$2][0]++
}
`},
}

func TestCheckValidPrograms(t *testing.T) {
	for _, tc := range checkerValidPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := Parse(tc.name, strings.NewReader(tc.program))
			s := Sexp{}
			s.emitTypes = true
			t.Log(s.Dump(ast))
			if err != nil {
				s := Sexp{}
				s.emitTypes = true
				t.Fatal(s.Dump(ast))
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
	{"Int + Int -> Int",
		&binaryExprNode{lhs: &intConstNode{position{}, 1},
			rhs: &intConstNode{position{}, 1},
			op:  PLUS},
		Int,
	},
	// TODO(jaq): subtypes
	// {"Int + Float -> Float",
	// 	&binaryExprNode{lhs: &intConstNode{position{}, 1},
	// 		rhs: &floatConstNode{position{}, 1.0},
	// 		op:  PLUS},
	// 	Float,
	// },
	{"⍺ + Float -> Float",
		&binaryExprNode{lhs: &idNode{pos: position{}, sym: &Symbol{Name: "i", Kind: VarSymbol, Type: NewTypeVariable()}},
			rhs: &caprefNode{pos: position{}, sym: &Symbol{Kind: CaprefSymbol, Type: Float}},
			op:  PLUS},
		Float,
	},
}

func TestCheckTypeExpressions(t *testing.T) {
	defaultCompareUnexportedFields := deep.CompareUnexportedFields
	deep.CompareUnexportedFields = true
	defer func() { deep.CompareUnexportedFields = defaultCompareUnexportedFields }()

	for _, tc := range checkerTypeExpressionTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			err := Check(tc.expr)
			if err != nil {
				t.Fatalf("check error: %s", err)
			}

			diff := deep.Equal(tc.expected, tc.expr.Type().Root())
			if len(diff) > 0 {
				t.Error(diff)
				s := Sexp{}
				s.emitTypes = true
				t.Log("Typed AST:\n" + s.Dump(tc.expr))
			}
		})
	}
}
