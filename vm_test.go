// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
	"regexp"
	"strings"
	"testing"
	"time"
)

type in_out struct {
	input string // no newlines
	ok    bool
}

type testProgram struct {
	name   string
	source string
	prog   []instr
	io     []in_out
}

var programs = []testProgram{
	{"simple line counter",
		"/$/ { inc(line-count) }",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{push, 0},
			instr{inc, 1},
			instr{ret, 1}},
		[]in_out{in_out{"", true}}},
	{"count a",
		"/a$/ { inc(a-count) }",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{push, 0},
			instr{inc, 1},
			instr{ret, 1}},
		[]in_out{
			in_out{"", false},
			in_out{"a", true}}},
	{"strptime and capref",
		"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")" +
			"inc(foo) }",
		[]instr{
			instr{match, 0},
			instr{jnm, 8},
			instr{capref, 1},
			instr{load, 0},
			instr{strptime, 2},
			instr{push, 0},
			instr{inc, 1},
			instr{ret, 1}},
		[]in_out{
			in_out{"2006-01-02T15:04:05", true}}},
	{"strptime and named capref",
		"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")" +
			"inc(foo) }",
		[]instr{
			instr{match, 0},
			instr{jnm, 8},
			instr{capref, 1},
			instr{load, 0},
			instr{strptime, 2},
			instr{push, 0},
			instr{inc, 1},
			instr{ret, 1}},
		[]in_out{
			in_out{"2006-01-02T15:04:05", true}}},
	{"inc by and set",
		"/(.*)/ {\n" +
			"inc(foo, $1)\n" +
			"set(bar, $1)\n" +
			"}",
		[]instr{
			instr{match, 0},
			instr{jnm, 9},
			instr{push, 0},
			instr{capref, 1},
			instr{inc, 2},
			instr{push, 1},
			instr{capref, 1},
			instr{set, 2},
			instr{ret, 1}},
		[]in_out{
			in_out{"37", true}}},
}

func TestCompile(t *testing.T) {
	for _, tc := range programs {
		metrics = make([]*Metric, 0)
		v, err := Compile(tc.name, strings.NewReader(tc.source))
		if err != nil {
			t.Errorf("Compile errors: %q", err)
			continue
		}
		if !reflect.DeepEqual(tc.prog, v.prog) {
			t.Errorf("%s: VM prog doesn't match.\n\texpected: %q\n\treceived: %q\n",
				tc.name, tc.prog, v.prog)
		}
	}
}

func TestRun(t *testing.T) {
	for _, tc := range programs {
		for _, i := range tc.io {
			v, err := Compile(tc.name, strings.NewReader(tc.source))
			if err != nil {
				t.Errorf("Compile errors: %q", err)
				continue
			}
			r := v.Run(i.input)
			if r != i.ok {
				t.Errorf("%s: Unexpected result after running on test input %q\n\texpected %v\n\treceived: %v", tc.name, i.input, i.ok, r)
			}
		}
	}
}

type instrTest struct {
	name  string
	prog  []instr
	re    []*regexp.Regexp
	str   []string
	stack []interface{}

	expected_stack  []interface{}
	expected_thread thread
}

var instructions = []instrTest{
	{"inc",
		[]instr{instr{inc, 1}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{},
		thread{pc: 1},
	},
	{"inc by int",
		[]instr{instr{inc, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, 1}, // first is metric 0 "foo", srcond is the inc val.
		[]interface{}{},
		thread{pc: 1},
	},
	{"inc by string",
		[]instr{instr{inc, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, "1"}, // first is metric 0 "foo", srcond is the inc val.
		[]interface{}{},
		thread{pc: 1},
	},
	{"set int",
		[]instr{instr{set, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 1}, // set metric 1 "bar"
		[]interface{}{},
		thread{pc: 1},
	},
	{"set str",
		[]instr{instr{set, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "1"},
		[]interface{}{},
		thread{pc: 1},
	},

	// {"match",
	// 	[]instr{instr{match, 0}},
	// 	[]*regexp.Regexp{regexp.MustCompile("a*b")},
	// 	[]string{},
	// 	[]interface{}{},
	// 	[]interface{}{},
	// 	thread{reg: 1, pc: 1, matches: []string{"aaaab"}},
	// },
	{"jnm",
		[]instr{instr{jnm, 37}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37}},
	{"strptime",
		[]instr{instr{strptime, 0}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 1, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC)}},
}

// TestInstrs tests that each instruction behaves as expected through one execution cycle.
func TestInstrs(t *testing.T) {
	for _, tc := range instructions {
		metrics = []*Metric{
			&Metric{Name: "foo", Type: Counter},
			&Metric{Name: "bar", Type: Gauge},
		}
		v := &vm{prog: tc.prog,
			re:    tc.re,
			str:   tc.str,
			stack: tc.stack}
		v.execute(&v.t, v.prog[0], "aaaab")
		if !reflect.DeepEqual(tc.expected_stack, v.stack) {
			t.Errorf("%s: unexpected virtual machine stack state.\n\texpected: %q\n\treceived: %q", tc.name, tc.stack, v.stack)
		}
		if !reflect.DeepEqual(tc.expected_thread, v.t) {
			t.Errorf("%s: unexpected virtual machine thread state.\n\texpected: %q\n\treceived: %q", tc.name, tc.expected_thread, v.t)
		}

	}
}
