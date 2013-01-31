// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
	"strings"
	"testing"
)

type validProgram struct {
	name    string
	program string
}

var kMtailPrograms = []validProgram{
	{"empty",
		""},

	{"declare counter",
		"counter line_count"},

	{"declare counter string name",
		"counter line_count as \"line-count\""},

	{"declare dimensioned counter",
		"counter foo by bar"},

	{"declare multi-dimensioned counter",
		"counter foo by bar, baz, quux"},

	{"declare hidden counter",
		"hidden counter foo"},

	{"simple pattern action",
		"/foo/ {}"},

	{"more complex action, calling builtin",
		"counter line_count\n" +
			"/foo/ {\n" +
			"  line_count++\n" +
			"}"},

	{"regex match includes escaped slashes",
		"counter foo\n" +
			"/foo\\// { foo++ }"},

	{"numeric capture group reference",
		"/(foo)/ {\n" +
			"  $1++\n" +
			"}"},

	{"strptime and capref",
		"/(.*)/ {\n" +
			"strptime($1, \"2006-01-02T15:04:05Z07:00\")\n" +
			" }"},

	{"named capture group reference",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"}"},

	{"nested match conditions",
		"counter foo\n" +
			"counter bar\n" +
			"/match(\\d+)/ {\n" +
			"  foo += $1\n" +
			"  /^bleh (\\S+)/ {\n" +
			"    bar++\n" +
			"    $1++\n" +
			"  }\n" +
			"}\n"},

	{"nested scope",
		"counter foo\n" +
			"/fo(o)/ {\n" +
			"  $1++\n" +
			"  /bar(xxx)/ {\n" +
			"    $1 += $1\n" +
			"    foo = $1\n" +
			"  }\n" +
			"}\n"},

	{"comment then code",
		"# %d [%p]\n" +
			"/^(?P<date>\\d+\\/\\d+\\/\\d+ \\d+:\\d+:\\d+) \\[(?P<pid>\\d+)\\] / {\n" +
			"  strptime($1, \"2006/01/02 15:04:05\")\n" +
			"}\n"},

	{"assignment",
		"counter variable\n" +
			"/(?P<foo>.*)/ {\n" +
			"variable = $foo\n" +
			"}\n"},

	{"increment operator",
		"counter var\n" +
			"/foo/ {\n" +
			"  var++\n" +
			"}\n"},

	{"incby operator",
		"counter var\n" +
			"/foo/ {\n  var += 2\n}\n"},

	{"additive",
		"counter time_total\n" +
			"/(?P<foo>.*)/ {\n" +
			"  timestamp() - time_total\n" +
			"}\n"},

	{"additive and mem storage",
		"counter time_total\n" +
			"counter variable by foo\n" +
			"/(?P<foo>.*)/ {\n" +
			"  time_total += timestamp() - variable[$foo]\n" +
			"}\n"},

	{"conditional expressions",
		"counter foo\n" +
			"/(?P<foo>.*)/ {\n" +
			"  $foo > 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo >= 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo < 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo <= 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo == 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo != 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"}\n"},

	{"decorator definition and invocation",
		"def foo { next }\n" +
			"@foo { }\n",
	},

	{"const regex",
		"const X /foo/\n" +
			"/foo / + X + / bar/ {\n" +
			"}\n",
	},

	{"multiline regex",
		"/foo / +\n" +
			"/barrr/ {\n" +
			"}\n",
	},

	{"len",
		"/(?P<foo>foo)/ {\n" +
			"len($foo) > 0 {\n" +
			"}\n" +
			"}\n",
	},
}

func TestParserRoundTrip(t *testing.T) {
	for _, tc := range kMtailPrograms {
		p := NewParser(tc.name, strings.NewReader(tc.program))
		//EmtailDebug = 999 // All the debugging.
		r := EmtailParse(p)

		if r != 0 || p.root == nil || len(p.errors) > 0 {
			t.Errorf("1st pass parse errors:\n")
			for _, e := range p.errors {
				t.Errorf("\t%s\n", e)
			}
			continue
		}

		u := Unparser{}
		output := u.Unparse(p.root)

		p2 := NewParser(tc.name+" 2", strings.NewReader(output))
		r = EmtailParse(p2)
		if r != 0 || p2.root == nil || len(p2.errors) > 0 {
			t.Errorf("2nd pass parse errors:\n")
			for _, e := range p2.errors {
				t.Errorf("\t%s\n", e)
			}
			continue
		}

		u = Unparser{}
		output2 := u.Unparse(p2.root)

		if !reflect.DeepEqual(output, output2) {
			t.Errorf("Round trip failed to generate same output.\n1: %s\n2: %s\n", output, output)
		}
	}
}

type InvalidProgram struct {
	name    string
	program string
	errors  []string
}

var InvalidPrograms = []InvalidProgram{
	{"unknown character",
		"?\n",
		[]string{"unknown character:1:1: Unexpected input: '?'"}},

	{"unterminated regex",
		"/foo\n",
		[]string{"unterminated regex:1:1-4: Unterminated regular expression: \"/foo\""}},

	{"invalid regex",
		"/foo(/\n",
		[]string{"invalid regex:1:1-6: error parsing regexp: missing closing ): `foo(`",
			"invalid regex:2:1: syntax error"}},

	{"invalid regex 2",
		"/blurg(?P<x.)/\n",
		[]string{"invalid regex 2:1:1-14: error parsing regexp: invalid named capture: `(?P<x.)`",
			"invalid regex 2:2:1: syntax error"}},

	{"invalid regex 3",
		"/blurg(?P<x>[[:alph:]])/\n",
		[]string{"invalid regex 3:1:1-24: error parsing regexp: invalid character class range: `[:alph:]`",
			"invalid regex 3:2:1: syntax error"}},

	{"unterminated string",
		" \"foo }\n",
		[]string{"unterminated string:1:2-7: Unterminated quoted string: \"\\\"foo }\""}},

	{"undefined named capture group",
		"/blurgh/ { $undef++ }\n",
		[]string{"undefined named capture group:1:12-17: Capture group $undef not defined by prior regular expression in this or an outer scope"}},

	{"out of bounds capref",
		"/(blyurg)/ { $2++ }\n",
		[]string{"out of bounds capref:1:14-15: Capture group $2 not defined by prior regular expression " +
			"in this or an outer scope"},
	},

	{"undefined decorator",
		"@foo {}\n",
		[]string{"undefined decorator:1:7: Decorator foo not defined"}},

	{"unterminated const regex",
		"const X /(?P<foo>",
		[]string{"unterminated const regex:1:9-17: Unterminated regular expression: \"/(?P<foo>\"",
			"unterminated const regex:1:9-17: syntax error"}},

	{"undefined const regex",
		"/foo / + X + / bar/ {}\n",
		[]string{"undefined const regex:1:10: Constant 'X' not defined."}},
}

func TestInvalidPrograms(t *testing.T) {
	for _, tc := range InvalidPrograms {
		p := NewParser(tc.name, strings.NewReader(tc.program))
		//EmtailDebug = 999 // All the debugging.
		EmtailParse(p)

		if !reflect.DeepEqual(tc.errors, p.errors) {
			t.Errorf("Incorrect error for '%s'\n\treceived: %q\n\texpected: %q\n", tc.name, p.errors, tc.errors)
		}
	}
}
