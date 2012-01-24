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

	{"simple pattern action",
		"/foo/ {}"},

	{"more complex action, calling builtin",
		"/foo/ {\n" +
			"  inc(line-count)\n" +
			"}"},

	{"regex match includes escaped slashes",
		"/foo\\// { inc(foo) }"},

	{"numeric capture group reference",
		"/(foo)/ {\n" +
			"  inc($1)\n" +
			"}"},

	{"strptime and capref",
		"/(.*)/ {\n" +
			"strptime($1, \"2006-01-02T15:04:05Z07:00\")\n" +
			"inc(foo)\n" +
			" }"},

	{"named capture group reference",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  inc(foo)\n" +
			"}"},

	{"nested match conditions",
		"/match(\\d+)/ {\n" +
			"  inc(foo, $1)\n" +
			"  /^bleh (\\S+)/ {\n" +
			"    inc(bar)\n" +
			"    inc($1)\n" +
			"  }\n" +
			"}\n"},

	{"nested scope",
		"/fo(o)/ {\n" +
			"  inc($1)\n" +
			"  /bar(xxx)/ {\n" +
			"    inc($1, $1)\n" +
			"    set(foo, $1)\n" +
			"  }\n" +
			"}\n"},

	{"comment then code",
		"# %d [%p]\n" +
			"/^(?P<date>\\d+\\/\\d+\\/\\d+ \\d+:\\d+:\\d+) \\[(?P<pid>\\d+)\\] / {\n" +
			"  strptime($1, \"2006/01/02 15:04:05\")\n" +
			"  tag(\"pid\", $2)\n" +
			"  inc(transfers_total)\n" +
			"}\n"},
}

func TestParserRoundTrip(t *testing.T) {
	for _, tc := range kMtailPrograms {
		metrics = make([]*Metric, 0)
		p := NewParser(tc.name, strings.NewReader(tc.program))
		//EmtailDebug = 1 //999 // All the debugging.
		EmtailParse(p)

		if p.root == nil || len(p.errors) > 0 {
			t.Errorf("parse errors:\n")
			for _, e := range p.errors {
				t.Errorf("\t%s\n", e)
			}
			continue
		}

		up := &unparser{}
		p.root.acceptVisitor(up)

		p2 := NewParser(tc.name+" 2", strings.NewReader(up.output))
		EmtailParse(p2)
		if p2.root == nil || len(p2.errors) > 0 {
			t.Errorf("Errors parsing %s 2:\n%q\n", tc.name, p2.errors)
			continue
		}

		up2 := &unparser{}
		p2.root.acceptVisitor(up2)

		if !reflect.DeepEqual(up, up2) {
			t.Errorf("Round trip failed to generate same output.\nup: %s\nup2: %s\n", up.output, up2.output)
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
		" inc(\"foo) }\n",
		[]string{"unterminated string:1:6-12: Unterminated quoted string: \"\\\"foo) }\"",
			"unterminated string:1:6-12: syntax error"}},

	{"undefined named capture group",
		"/blurgh/ { inc($undef) }\n",
		[]string{"undefined named capture group:1:16-21: Capture group $undef not defined by prior regular expression in this or an outer scope"}},

	{"out of bounds capref",
		"/(blyurg)/ { inc($2) }\n",
		[]string{"out of bounds capref:1:18-19: Capture group $2 not defined by prior regular expression " +
			"in this or an outer scope"},
	},
}

func TestInvalidPrograms(t *testing.T) {
	for _, tc := range InvalidPrograms {
		p := NewParser(tc.name, strings.NewReader(tc.program))
		EmtailParse(p)

		if !reflect.DeepEqual(tc.errors, p.errors) {
			t.Errorf("Incorrect error for %s\n\texpected: %q\n\treceived: %q\n", tc.name, tc.errors, p.errors)
		}
	}
}
