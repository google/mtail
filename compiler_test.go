// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

type in_out struct {
	input string // no newlines
	ok    bool
}

// debug print for instructions
func (i instr) String() string {
	return fmt.Sprintf("{%s %d}", opNames[i.op], i.opnd)
}

var programs = []struct {
	name   string
	source string
	prog   []instr // expected bytecode 
}{
	// Composite literals require too many explicit conversions.
	{"simple line counter",
		"counter line_count\n/$/ { line_count++ }",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++ }",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")" +
			"foo++ }",
		[]instr{
			instr{match, 0},
			instr{jnm, 9},
			instr{push, 0},
			instr{capref, 0},
			instr{str, 0},
			instr{strptime, 2},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")" +
			"foo++ }",
		[]instr{
			instr{match, 0},
			instr{jnm, 9},
			instr{push, 0},
			instr{capref, 1},
			instr{str, 0},
			instr{strptime, 2},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/(.*)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}",
		[]instr{
			instr{match, 0},
			instr{jnm, 12},
			instr{mload, 0},
			instr{dload, 0},
			instr{push, 0},
			instr{capref, 0},
			instr{inc, 1},
			instr{mload, 1},
			instr{dload, 0},
			instr{push, 0},
			instr{capref, 0},
			instr{set, 0}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jnm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, -1},
			instr{jnm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 0},
			instr{jnm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, -1},
			instr{jm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 0},
			instr{jm, 7},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"nested cond",
		"counter foo\n" +
			"/(.*)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}",
		[]instr{
			instr{match, 0},
			instr{jnm, 10},
			instr{push, 0},
			instr{capref, 0},
			instr{push, 1},
			instr{cmp, 1},
			instr{jm, 10},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"deco",
		"counter foo\n" +
			"counter bar\n" +
			"def foo {\n" +
			"  /.*/ {\n" +
			"    foo++\n" +
			"    next\n" +
			"  }\n" +
			"}\n" +
			"" +
			"@foo { bar++ }\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 8},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0},
			instr{mload, 1},
			instr{dload, 0},
			instr{inc, 0}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]instr{
			instr{str, 0},
			instr{length, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jnm, 5}}},
}

func TestCompile(t *testing.T) {
	for _, tc := range programs {
		// Wipe metrics
		metrics = make([]*Metric, 0)
		v, err := Compile(tc.name, strings.NewReader(tc.source))
		if err != nil {
			t.Errorf("Compile errors: %q", err)
			continue
		}
		if !reflect.DeepEqual(tc.prog, v.prog) {
			t.Errorf("%s: VM prog doesn't match.\n\texpected: %v\n\treceived: %v",
				tc.name, tc.prog, v.prog)
		}
	}
}
