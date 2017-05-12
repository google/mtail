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
		t.Logf("Starting %s", tc.name)
		ast, err := Parse(tc.name, strings.NewReader(tc.program))
		if err != nil {
			t.Fatal(err)
		}
		err = Check(ast)
		if err == nil {
			t.Errorf("Error should not be nil for invalid program %q", tc.name)
			continue
		}

		diff := deep.Equal(
			strings.Join(tc.errors, "\n"),        // want
			strings.TrimRight(err.Error(), "\n")) // got
		if diff != nil {
			t.Errorf("Incorrect error for %q\n%s", tc.name, diff)
		}
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
		t.Logf("Starting %s", tc.name)
		ast, err := Parse(tc.name, strings.NewReader(tc.program))
		if err != nil {
			t.Fatal(err)
		}
		err = Check(ast)
		if err != nil {
			t.Errorf("Checker failed for valid program %q:\n%s", tc.name, err)
		}
	}
}
