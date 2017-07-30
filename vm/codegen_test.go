// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	"github.com/go-test/deep"
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
			instr{match, 0},
			instr{jnm, 7},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 7},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 11},
			instr{setmatched, false},
			instr{push, 0},
			instr{capref, 1},
			instr{str, 0},
			instr{strptime, 2},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 11},
			instr{setmatched, false},
			instr{push, 0},
			instr{capref, 1},
			instr{str, 0},
			instr{strptime, 2},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 17},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{mload, 0},
			instr{dload, 0},
			instr{push, 0},
			instr{capref, 1},
			instr{iadd, nil},
			instr{iset, nil},
			instr{mload, 1},
			instr{dload, 0},
			instr{push, 0},
			instr{capref, 1},
			instr{iset, nil},
			instr{setmatched, true}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jnm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, -1},
			instr{jnm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 0},
			instr{jnm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, -1},
			instr{jm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 0},
			instr{jm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"nested cond",
		"counter foo\n" +
			"/(.*)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]instr{
			instr{match, 0},
			instr{jnm, 14},
			instr{setmatched, false},
			instr{push, 0},
			instr{capref, 1},
			instr{push, 1},
			instr{cmp, 1},
			instr{jm, 13},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true},
			instr{setmatched, true}}},
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
			instr{match, 0},
			instr{jnm, 10},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{mload, 1},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]instr{
			instr{str, 0},
			instr{length, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jnm, 7},
			instr{setmatched, false},
			instr{setmatched, true}}},
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]instr{
			instr{push, 1},
			instr{push, 7},
			instr{and, nil},
			instr{push, 15},
			instr{xor, nil},
			instr{push, 8},
			instr{or, nil},
			instr{push, 16},
			instr{not, nil},
			instr{push, 2},
			instr{shl, nil},
			instr{push, 1},
			instr{push, 20},
			instr{shr, nil}}},
	{"pow", `
/(\d+) (\d+)/ {
$1 ** $2
}
`,
		[]instr{
			instr{match, 0},
			instr{jnm, 9},
			instr{setmatched, false},
			instr{push, 0},
			instr{capref, 1},
			instr{push, 0},
			instr{capref, 2},
			instr{ipow, nil},
			instr{setmatched, true}}},
	{"indexed expr", `
counter a by b
a["string"]++
`,
		[]instr{
			instr{str, 0},
			instr{mload, 0},
			instr{dload, 1},
			instr{inc, nil}}},
	{"strtol", `
strtol("deadbeef", 16)
`,
		[]instr{
			instr{str, 0},
			instr{push, 16},
			instr{strtol, 2}}},
	{"float", `
20.0
`,
		[]instr{
			instr{push, 20.0}}},
	{"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]instr{
			instr{otherwise, nil},
			instr{jnm, 7},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true}}},
	{"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]instr{
			instr{push, 1},
			instr{push, 0},
			instr{cmp, 1},
			instr{jnm, 10},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{inc, nil},
			instr{setmatched, true},
			instr{jmp, 13},
			instr{mload, 1},
			instr{dload, 0},
			instr{inc, nil},
		},
	},
	{"mod",
		`
3 % 1
`,
		[]instr{
			instr{push, 3},
			instr{push, 1},
			instr{imod, nil},
		},
	},
	{"del", `
counter a by b
del a["string"]
`,
		[]instr{
			instr{str, 0},
			instr{mload, 0},
			instr{del, 1}},
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
			instr{match, 0},
			instr{jnm, 9},
			instr{setmatched, false},
			instr{mload, 0},
			instr{dload, 0},
			instr{push, 0},
			instr{capref, 1},
			instr{iset, nil},
			instr{setmatched, true},
			instr{match, 1},
			instr{jnm, 18},
			instr{setmatched, false},
			instr{mload, 1},
			instr{dload, 0},
			instr{push, 1},
			instr{capref, 1},
			instr{fset, nil},
			instr{setmatched, true},
		},
	},

	{"getfilename", `
getfilename()
`,
		[]instr{
			instr{getfilename, nil},
		},
	},
}

func TestCodegen(t *testing.T) {
	defaultCompareUnexportedFields := deep.CompareUnexportedFields
	deep.CompareUnexportedFields = true
	defer func() { deep.CompareUnexportedFields = defaultCompareUnexportedFields }()

	for _, tc := range testCodeGenPrograms {
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
			obj, err := CodeGen(tc.name, ast)
			if err != nil {
				t.Errorf("Compile errors:\n%s", err)
				s := Sexp{}
				s.emitTypes = true
				t.Fatalf("AST:\n%s", s.Dump(ast))
			}

			if diff := deep.Equal(tc.prog, obj.prog); diff != nil {
				t.Error(diff)
				t.Logf("Expected:\n%s\nReceived:\n%s", tc.prog, obj.prog)
			}
		})
	}
}
