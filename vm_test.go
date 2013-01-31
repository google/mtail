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
	i              instr
	re             []*regexp.Regexp
	str            []string
	reversed_stack []interface{} // stack is inverted to be pushed onto vm stack

	expected_stack  []interface{}
	expected_thread thread
}{
	// Composite literals require too many explicit conversions.
	{"inc",
		instr{inc, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}},
	},
	{"inc by int",
		instr{inc, 2},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, 1}, // first is metric 0 "foo", second is the inc val.
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}},
	},
	{"inc by string",
		instr{inc, 2},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0, "1"}, // first is metric 0 "foo", second is the inc val.
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}},
	},
	{"set int",
		instr{set, 2},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2}, // set metric 1 "bar"
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}},
	},
	{"set str",
		instr{set, 2},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}},
	},
	{"match",
		instr{match, 0},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{match: true, pc: 0, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"cmp lt",
		instr{cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{},
		thread{pc: 0, match: true, matches: map[int][]string{}}},
	{"cmp eq",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2", "2"},
		[]interface{}{},
		thread{pc: 0, match: true, matches: map[int][]string{}}},
	{"cmp gt",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{},
		thread{pc: 0, match: true, matches: map[int][]string{}}},
	{"cmp le",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, "2"},
		[]interface{}{},
		thread{pc: 0, match: false, matches: map[int][]string{}}},
	{"cmp ne",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2"},
		[]interface{}{},
		thread{pc: 0, match: false, matches: map[int][]string{}}},
	{"cmp ge",
		instr{cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{},
		thread{pc: 0, match: false, matches: map[int][]string{}}},
	{"jnm",
		instr{jnm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"jm",
		instr{jm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}}},
	{"strptime",
		instr{strptime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 0, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"add",
		instr{add, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"sub",
		instr{sub, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"tolower",
		instr{tolower, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"mIxeDCasE"},
		[]interface{}{"mixedcase"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length",
		instr{length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1234"},
		[]interface{}{4},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length 0",
		instr{length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{""},
		[]interface{}{0},
		thread{pc: 0, matches: map[int][]string{}}},
}

// TestInstrs tests that each instruction behaves as expected through one execution cycle.
func TestInstrs(t *testing.T) {
	for _, tc := range instructions {
		m := append(metrics,
			NewMetric("foo", "test", Counter),
			NewMetric("bar", "test", Counter))

		v := newVm(tc.name, tc.re, tc.str, m, []instr{tc.i})
		v.t = new(thread)
		v.t.stack = make([]interface{}, 0)
		for _, item := range tc.reversed_stack {
			v.t.Push(item)
		}
		v.t.matches = make(map[int][]string, 0)
		v.input = "aaaab"
		v.execute(v.t, tc.i)

		if !reflect.DeepEqual(tc.expected_stack, v.t.stack) {
			t.Errorf("%s: unexpected virtual machine stack state.\n\texpected: %q\n\treceived: %q", tc.name, tc.expected_stack, v.t.stack)
		}
		tc.expected_thread.stack = tc.expected_stack
		if !reflect.DeepEqual(&tc.expected_thread, v.t) {
			t.Errorf("%s: unexpected virtual machine thread state.\n\texpected: %q\n\treceived: %q", tc.name, tc.expected_thread, v.t)
		}
	}
}
