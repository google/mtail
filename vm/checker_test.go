// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

type checkerInvalidProgram struct {
	name    string
	program string
	errors  []string
}

var checkerInvalidPrograms = []checkerInvalidProgram{
	{"undefined named capture group",
		"/blurgh/ { $undef++\n }\n",
		[]string{":1:1: Capture group `$undef' was not defined by a regular expression in this or outer scopes.\n\tTry using `(?P<undef>...)' to name the capture group."}},

	{"out of bounds capref",
		"/(blyurg)/ { $2++ \n}\n",
		[]string{":1:1: Capture group `$2' was not defined by a regular expression " +
			"in this or outer scopes.\n\tTry using `(?P<2>...)' to name the capture group."},
	},

	{"undefined decorator",
		"@foo {}\n",
		[]string{":1:1: Decorator `foo' not defined.\n\tTry adding a definition `def foo {}' earlier in the program."}},

	{"undefined identifier",
		"// { x++ \n}\n",
		[]string{":1:1: Identifier `x' not declared.\n\tTry adding `counter x' to the top of the program."},
	},
}

func TestCheckInvalidPrograms(t *testing.T) {
	for _, tc := range checkerInvalidPrograms {
		ast, err := Parse(tc.name, strings.NewReader(tc.program), metrics.NewStore())
		if err != nil {
			t.Fatal(err)
		}
		err = Check(ast)
		if err == nil {
			t.Errorf("Error should not be nil for invalid program %q", tc.name)
			continue
		}

		diff := pretty.Compare(
			strings.Join(tc.errors, "\n"),        // want
			strings.TrimRight(err.Error(), "\n")) // got
		if len(diff) > 0 {
			t.Errorf("Incorrect error for %q\n%s", tc.name, diff)
		}
	}
}
