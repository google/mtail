// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

type validProgram struct {
	name    string
	program string
}

var mtailPrograms = []validProgram{
	{"empty",
		""},

	{"newline",
		"\n"},

	{"declare counter",
		"counter line_count\n"},

	{"declare counter string name",
		"counter line_count as \"line-count\"\n"},

	{"declare dimensioned counter",
		"counter foo by bar\n"},

	{"declare multi-dimensioned counter",
		"counter foo by bar, baz, quux\n"},

	{"declare hidden counter",
		"hidden counter foo\n"},

	{"declare gauge",
		"gauge foo\n"},

	{"declare timer",
		"timer foo\n"},

	{"simple pattern action",
		"/foo/ {}\n"},

	{"more complex action, increment counter",
		"counter line_count\n" +
			"/foo/ {\n" +
			"  line_count++\n" +
			"}\n"},

	{"regex match includes escaped slashes",
		"counter foo\n" +
			"/foo\\// { foo++\n}\n"},

	{"numeric capture group reference",
		"/(foo)/ {\n" +
			"  $1++\n" +
			"}\n"},

	{"strptime and capref",
		"/(.*)/ {\n" +
			"strptime($1, \"2006-01-02T15:04:05Z07:00\")\n" +
			" }\n"},

	{"named capture group reference",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"}\n"},

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

	{"multiplicative",
		"counter a\n" +
			"counter b\n" +
			"   /foo/ {\n   a * b\n" +
			"      a ** b\n" +
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
		"def foo { next\n }\n" +
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

	{"def and next",
		"def foobar {/(?P<date>.*)/ {" +
			"  next" +
			"}" +
			"}",
	},

	{"const",
		`const IP /\d+(\.\d+){3}/`},

	{"bitwise",
		`/foo(\d)/ {
  $1 & 7
  $1 | 8
  $1 << 4
  $1 >> 20
  $1 ^ 15
  ~ 1
}`},
	{"floats",
		`gauge foo
/foo/ {
foo = 3.14
}`},
	{"simple otherwise action",
		"otherwise {}\n"},

	{"pattern action then otherwise action",
		`counter line_count by type
		/foo/ {
			line_count["foo"]++
		}
		otherwise {
			line_count["misc"] += 10
		}`},
	{"simple else clause",
		"/foo/ {} else {}"},
	{"nested else clause",
		"/foo/ { / bar/ {}  } else { /quux/ {} else {} }"},
}

func TestParserRoundTrip(t *testing.T) {
	for _, tc := range mtailPrograms {
		p := newParser(tc.name, strings.NewReader(tc.program), metrics.NewStore())
		r := mtailParse(p)

		if r != 0 || p.root == nil || len(p.errors) > 0 {
			t.Errorf("1st pass parse errors:\n")
			for _, e := range p.errors {
				t.Errorf("\t%s\n", e)
			}
			continue
		}

		u := Unparser{}
		output := u.Unparse(p.root)

		p2 := newParser(tc.name+" 2", strings.NewReader(output), metrics.NewStore())
		r = mtailParse(p2)
		if r != 0 || p2.root == nil || len(p2.errors) > 0 {
			t.Errorf("2nd pass parse errors:\n")
			for _, e := range p2.errors {
				t.Errorf("\t%s\n", e)
			}
			continue
		}

		u = Unparser{}
		output2 := u.Unparse(p2.root)

		diff := pretty.Compare(output2, output)
		if len(diff) > 0 {
			t.Errorf("Round trip failed to generate same output.\n%s", diff)
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
		[]string{"unterminated regex:1:2-4: Unterminated regular expression: \"/foo\"",
			"unterminated regex:1:2-4: syntax error"}},

	{"invalid regex",
		"/foo(/\n",
		[]string{"invalid regex:1:6: error parsing regexp: missing closing ): `foo(`",
			"invalid regex:2:7: syntax error"}},

	{"invalid regex 2",
		"/blurg(?P<x.)/\n",
		[]string{"invalid regex 2:1:14: error parsing regexp: invalid named capture: `(?P<x.)`",
			"invalid regex 2:2:15: syntax error"}},

	{"invalid regex 3",
		"/blurg(?P<x>[[:alph:]])/\n",
		[]string{"invalid regex 3:1:24: error parsing regexp: invalid character class range: `[:alph:]`",
			"invalid regex 3:2:25: syntax error"}},

	{"unterminated string",
		" \"foo }\n",
		[]string{"unterminated string:1:2-7: Unterminated quoted string: \"\\\"foo }\""}},

	{"undefined named capture group",
		"/blurgh/ { $undef++\n }\n",
		[]string{"undefined named capture group:1:12-17: Capture group $undef not defined by prior regular expression in this or an outer scope"}},

	{"out of bounds capref",
		"/(blyurg)/ { $2++ \n}\n",
		[]string{"out of bounds capref:1:14-15: Capture group $2 not defined by prior regular expression " +
			"in this or an outer scope"},
	},

	{"undefined decorator",
		"@foo {}\n",
		[]string{"undefined decorator:1:7: Decorator foo not defined"}},

	{"unterminated const regex",
		"const X /(?P<foo>",
		[]string{"unterminated const regex:1:10-17: Unterminated regular expression: \"/(?P<foo>\"",
			"unterminated const regex:1:10-17: syntax error"}},

	{"undefined const regex",
		"/foo / + X + / bar/ {}\n",
		[]string{"undefined const regex:1:10: Constant 'X' not defined."}},
}

func TestInvalidPrograms(t *testing.T) {
	for _, tc := range InvalidPrograms {
		p := newParser(tc.name, strings.NewReader(tc.program), metrics.NewStore())
		mtailParse(p)

		diff := pretty.Compare(
			strings.Join(tc.errors, "\n"),             // want
			strings.TrimRight(p.errors.Error(), "\n")) // got
		if len(diff) > 0 {
			t.Errorf("Incorrect error for '%s'\n%s", tc.name, diff)
		}
	}
}
