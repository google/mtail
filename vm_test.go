// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
	"strings"
	"testing"
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
			instr{inc, 0},
			instr{ret, 1}},
		[]in_out{in_out{"", true}}},
	{"count a",
		"/a$/ { inc(a-count) }",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{push, 0},
			instr{inc, 0},
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
			instr{strptime, 0},
			instr{push, 0},
			instr{inc, 0},
			instr{ret, 1}},
		[]in_out{
			in_out{"2006-01-02T15:04:05", true}}},
}

func TestCompile(t *testing.T) {
	for _, tc := range programs {
		metrics = make([]*metric, 0)
		v, err := Compile(tc.name, strings.NewReader(tc.source))
		if err != nil {
			t.Errorf("Compile errors: %q", err)
			continue
		}
		if !reflect.DeepEqual(tc.prog, v.prog) {
			t.Errorf("VM prog doesn't match.\n\texpected: %q\n\treceived: %q\n",
				tc.prog, v.prog)
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
