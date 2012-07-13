// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
	"regexp"
	"testing"
	"time"
)

var instructions = []struct {
	name           string
	prog           []instr
	re             []*regexp.Regexp
	str            []string
	reversed_stack []interface{} // stack is inverted to be pushed onto vm stack

	expected_stack  []interface{}
	expected_thread thread
}{
	{"inc",
		[]instr{instr{inc, 0}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{},
		thread{pc: 1, matches: map[int][]string{}},
	},
	{"inc by int",
		[]instr{instr{inc, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, 1}, // first is metric 0 "foo", second is the inc val.
		[]interface{}{},
		thread{pc: 1, matches: map[int][]string{}},
	},
	{"inc by string",
		[]instr{instr{inc, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, "1"}, // first is metric 0 "foo", second is the inc val.
		[]interface{}{},
		thread{pc: 1, matches: map[int][]string{}},
	},
	{"set int",
		[]instr{instr{set, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2}, // set metric 1 "bar"
		[]interface{}{},
		thread{pc: 1, matches: map[int][]string{}},
	},
	{"set str",
		[]instr{instr{set, 2}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{},
		thread{pc: 1, matches: map[int][]string{}},
	},
	{"match",
		[]instr{instr{match, 0}},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{reg: 1, pc: 1, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"jnm",
		[]instr{instr{jnm, 37}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"strptime",
		[]instr{instr{strptime, 0}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 1, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"add",
		[]instr{instr{add, 0}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 1, matches: map[int][]string{}}},
	{"sub",
		[]instr{instr{sub, 0}},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 1, matches: map[int][]string{}}},
}

// TestInstrs tests that each instruction behaves as expected through one execution cycle.
func TestInstrs(t *testing.T) {
	for _, tc := range instructions {
		metrics = []*Metric{
			&Metric{Name: "foo", Kind: Counter, D: &Datum{}},
			&Metric{Name: "bar", Kind: Gauge, D: &Datum{}},
		}

		// expected_stack := make([]interface{}, 0, 10)
		// for _, item := range tc.expected_stack {
		// 	expected_stack = append(expected_stack, item)
		// }

		v := &vm{prog: tc.prog,
			re:  tc.re,
			str: tc.str,
		}
		v.t.stack = make([]interface{}, 0)
		for _, item := range tc.reversed_stack {
			v.t.Push(item)
		}
		v.t.matches = make(map[int][]string, 0)
		v.execute(&v.t, v.prog[0], "aaaab")

		if !reflect.DeepEqual(tc.expected_stack, v.t.stack) {
			t.Errorf("%s: unexpected virtual machine stack state.\n\texpected: %q\n\treceived: %q", tc.name, tc.expected_stack, v.t.stack)
		}
		tc.expected_thread.stack = tc.expected_stack
		if !reflect.DeepEqual(tc.expected_thread, v.t) {
			t.Errorf("%s: unexpected virtual machine thread state.\n\texpected: %q\n\treceived: %q", tc.name, tc.expected_thread, v.t)
		}
	}
}
