// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"strings"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

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
		"counter line_count\n/$/ { line_count++\n }\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 5},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, 0}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
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
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
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
			"}\n",
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
			"}\n",
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
			"@foo { bar++\n }\n",
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
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]instr{
			instr{push, 1},
			instr{push, 7},
			instr{and, 0},
			instr{push, 15},
			instr{xor, 0},
			instr{push, 8},
			instr{or, 0},
			instr{push, 16},
			instr{not, 0},
			instr{push, 2},
			instr{shl, 0},
			instr{push, 1},
			instr{push, 20},
			instr{shr, 0}}},
}

func TestCompile(t *testing.T) {
	for _, tc := range programs {
		m := metrics.Store{}
		v, err := Compile(tc.name, strings.NewReader(tc.source), &m, false, true)
		if err != nil {
			t.Errorf("Compile errors: %q", err)
			continue
		}
		diff := pretty.Compare(tc.prog, v.prog)
		if len(diff) > 0 {
			t.Errorf("%s: VM prog doesn't match.\n%s", tc.name, diff)
		}
	}
}
