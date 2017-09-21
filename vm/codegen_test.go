// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	go_cmp "github.com/google/go-cmp/cmp"
)

var testCodeGenPrograms = []struct {
	name   string
	source string
	prog   []instr // expected bytecode
}{
	// Composite literals require too many explicit conversions.
	{"simple line counter",
		"counter line_count\n/$/ { line_count++\n }\n",
		[]instr{
			{match, 0},
			{jnm, 7},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]instr{
			{match, 0},
			{jnm, 7},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]instr{
			{match, 0},
			{jnm, 11},
			{setmatched, false},
			{push, 0},
			{capref, 1},
			{str, 0},
			{strptime, 2},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]instr{
			{match, 0},
			{jnm, 11},
			{setmatched, false},
			{push, 0},
			{capref, 1},
			{str, 0},
			{strptime, 2},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]instr{
			{match, 0},
			{jnm, 14},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{inc, 0},
			{mload, 1},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{iset, nil},
			{setmatched, true}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, 1},
			{jnm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, -1},
			{jnm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, 0},
			{jnm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, 1},
			{jm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, -1},
			{jm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, 0},
			{jm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"nested cond",
		"counter foo\n" +
			"/(.*)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]instr{
			{match, 0},
			{jnm, 14},
			{setmatched, false},
			{push, 0},
			{capref, 1},
			{push, int64(1)},
			{cmp, 1},
			{jm, 13},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true},
			{setmatched, true}}},
	{"deco",
		"counter foo\n" +
			"counter bar\n" +
			"def fooWrap {\n" +
			"  /.*/ {\n" +
			"    foo++\n" +
			"    next\n" +
			"  }\n" +
			"}\n" +
			"" +
			"@fooWrap { bar++\n }\n",
		[]instr{
			{match, 0},
			{jnm, 10},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{mload, 1},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]instr{
			{str, 0},
			{length, 1},
			{push, int64(0)},
			{cmp, 1},
			{jnm, 7},
			{setmatched, false},
			{setmatched, true}}},
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]instr{
			{push, int64(1)},
			{push, int64(7)},
			{and, nil},
			{push, int64(15)},
			{xor, nil},
			{push, int64(8)},
			{or, nil},
			{push, int64(16)},
			{not, nil},
			{push, int64(2)},
			{shl, nil},
			{push, int64(1)},
			{push, int64(20)},
			{shr, nil}}},
	{"pow", `
/(\d+) (\d+)/ {
$1 ** $2
}
`,
		[]instr{
			{match, 0},
			{jnm, 9},
			{setmatched, false},
			{push, 0},
			{capref, 1},
			{push, 0},
			{capref, 2},
			{ipow, nil},
			{setmatched, true}}},
	{"indexed expr", `
counter a by b
a["string"]++
`,
		[]instr{
			{str, 0},
			{mload, 0},
			{dload, 1},
			{inc, nil}}},
	{"strtol", `
strtol("deadbeef", 16)
`,
		[]instr{
			{str, 0},
			{push, int64(16)},
			{s2i, 2}}},
	{"float", `
20.0
`,
		[]instr{
			{push, 20.0}}},
	{"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]instr{
			{otherwise, nil},
			{jnm, 7},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true}}},
	{"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]instr{
			{push, int64(1)},
			{push, int64(0)},
			{cmp, 1},
			{jnm, 10},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{inc, nil},
			{setmatched, true},
			{jmp, 13},
			{mload, 1},
			{dload, 0},
			{inc, nil},
		},
	},
	{"mod",
		`
3 % 1
`,
		[]instr{
			{push, int64(3)},
			{push, int64(1)},
			{imod, nil},
		},
	},
	{"del", `
counter a by b
del a["string"]
`,
		[]instr{
			{str, 0},
			{mload, 0},
			{del, 1}},
	},
	{"types", `
gauge i
gauge f
/(\d+)/ {
 i = $1
}
/(\d+\.\d+)/ {
 f = $1
}
`,
		[]instr{
			{match, 0},
			{jnm, 9},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{iset, nil},
			{setmatched, true},
			{match, 1},
			{jnm, 18},
			{setmatched, false},
			{mload, 1},
			{dload, 0},
			{push, 1},
			{capref, 1},
			{fset, nil},
			{setmatched, true},
		},
	},

	{"getfilename", `
getfilename()
`,
		[]instr{
			{getfilename, 0},
		},
	},

	{"dimensioned counter",
		`counter c by a,b,c
/(\d) (\d) (\d)/ {
  c[$1,$2][$3]++
}
`,
		[]instr{
			{match, 0},
			{jnm, 13},
			{setmatched, false},
			{push, 0},
			{capref, 1},
			{push, 0},
			{capref, 2},
			{push, 0},
			{capref, 3},
			{mload, 0},
			{dload, 3},
			{inc, nil},
			{setmatched, true}}},
	{"string to int",
		`counter c
/(.*)/ {
  c = int($1)
}
`,
		[]instr{
			{match, 0},
			{jnm, 11},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{push, 10},
			{s2i, nil},
			{iset, nil},
			{setmatched, true}}},
	{"int to float",
		`counter c
/(\d)/ {
  c = float($1)
}
`,
		[]instr{
			{match, 0},
			{jnm, 10},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{i2f, nil},
			{fset, nil},
			{setmatched, true}}},
	{"string to float",
		`counter c
/(.*)/ {
  c = float($1)
}
`,
		[]instr{
			{match, 0},
			{jnm, 10},
			{setmatched, false},
			{mload, 0},
			{dload, 0},
			{push, 0},
			{capref, 1},
			{s2f, nil},
			{fset, nil},
			{setmatched, true}}},
}

func TestCodegen(t *testing.T) {
	for _, tc := range testCodeGenPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := Parse(tc.name, strings.NewReader(tc.source))
			if err != nil {
				t.Fatalf("Parse error: %s", err)
			}
			err = Check(ast)
			if err != nil {
				t.Fatalf("Check error: %s", err)
			}
			s := Sexp{}
			s.emitTypes = true
			t.Log("Typed AST:\n" + s.Dump(ast))
			obj, err := CodeGen(tc.name, ast)
			if err != nil {
				t.Fatalf("Codegen error:\n%s", err)
			}

			if diff := go_cmp.Diff(tc.prog, obj.prog, go_cmp.AllowUnexported(instr{})); diff != "" {
				t.Error(diff)
				t.Logf("Expected:\n%s\nReceived:\n%s", tc.prog, obj.prog)
			}
		})
	}
}
