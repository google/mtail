// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package codegen_test

import (
	"flag"
	"strings"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/runtime/code"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/checker"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/codegen"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/parser"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var codegenTestDebug = flag.Bool("codegen_test_debug", false, "Log ASTs and debugging information ")

var testCodeGenPrograms = []struct {
	name   string
	source string
	prog   []code.Instr // expected bytecode
}{
	// Composite literals require too many explicit conversions.
	{
		"simple line counter",
		"counter lines_total\n/$/ { lines_total++\n }\n",
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 7, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 1},
			{code.Dload, 0, 1},
			{code.Inc, nil, 1},
			{code.Setmatched, true, 1},
		},
	},
	{
		"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 7, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 1},
			{code.Dload, 0, 1},
			{code.Inc, nil, 1},
			{code.Setmatched, true, 1},
		},
	},
	{
		"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 11, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 1},
			{code.Capref, 1, 1},
			{code.Str, 0, 1},
			{code.Strptime, 2, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 11, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 1},
			{code.Capref, 1, 1},
			{code.Str, 0, 1},
			{code.Strptime, 2, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 16, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.S2i, nil, 3},
			{code.Inc, 0, 3},
			{code.Mload, 1, 4},
			{code.Dload, 0, 4},
			{code.Push, 0, 4},
			{code.Capref, 1, 4},
			{code.S2i, nil, 4},
			{code.Iset, nil, 4},
			{code.Setmatched, true, 2},
		},
	},
	{
		"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, 1, 1},
			{code.Jnm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, -1, 1},
			{code.Jnm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, 0, 1},
			{code.Jnm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, 1, 1},
			{code.Jm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, -1, 1},
			{code.Jm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1), 1},
			{code.Push, int64(0), 1},
			{code.Icmp, 0, 1},
			{code.Jm, 6, 1},
			{code.Push, true, 1},
			{code.Jmp, 7, 1},
			{code.Push, false, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"nested cond",
		"counter foo\n" +
			"/(\\d+)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 19, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2i, nil, 2},
			{code.Push, int64(1), 2},
			{code.Icmp, 1, 2},
			{code.Jm, 11, 2},
			{code.Push, true, 2},
			{code.Jmp, 12, 2},
			{code.Push, false, 2},
			{code.Jnm, 18, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Inc, nil, 3},
			{code.Setmatched, true, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"deco",
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
		[]code.Instr{
			{code.Match, 0, 3},
			{code.Jnm, 10, 3},
			{code.Setmatched, false, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Inc, nil, 4},
			{code.Mload, 1, 8},
			{code.Dload, 0, 8},
			{code.Inc, nil, 8},
			{code.Setmatched, true, 3},
		},
	},
	{
		"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]code.Instr{
			{code.Str, 0, 0},
			{code.Length, 1, 0},
			{code.Push, int64(0), 0},
			{code.Cmp, 1, 0},
			{code.Jnm, 7, 0},
			{code.Push, true, 0},
			{code.Jmp, 8, 0},
			{code.Push, false, 0},
			{code.Jnm, 11, 0},
			{code.Setmatched, false, 0},
			{code.Setmatched, true, 0},
		},
	},
	{
		"bitwise", `
gauge a

a = 1 & 7 ^ 15 | 8
a = ~ 16 << 2
a = 1 >> 20
`,
		[]code.Instr{
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, int64(1), 3},
			{code.Push, int64(7), 3},
			{code.And, nil, 3},
			{code.Push, int64(15), 3},
			{code.Xor, nil, 3},
			{code.Push, int64(8), 3},
			{code.Or, nil, 3},
			{code.Iset, nil, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Push, int64(16), 4},
			{code.Neg, nil, 4},
			{code.Push, int64(2), 4},
			{code.Shl, nil, 4},
			{code.Iset, nil, 4},
			{code.Mload, 0, 5},
			{code.Dload, 0, 5},
			{code.Push, int64(1), 5},
			{code.Push, int64(20), 5},
			{code.Shr, nil, 5},
			{code.Iset, nil, 5},
		},
	},
	{
		"pow", `
gauge a
/(\d+) (\d+)/ {
  a = $1 ** $2
}
`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 14, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.S2i, nil, 3},
			{code.Push, 0, 3},
			{code.Capref, 2, 3},
			{code.S2i, nil, 3},
			{code.Ipow, nil, 3},
			{code.Iset, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"indexed expr", `
counter a by b
a["string"]++
`,
		[]code.Instr{
			{code.Str, 0, 2},
			{code.Mload, 0, 2},
			{code.Dload, 1, 2},
			{code.Inc, nil, 2},
		},
	},
	{
		"strtol", `
strtol("deadbeef", 16)
`,
		[]code.Instr{
			{code.Str, 0, 1},
			{code.Push, int64(16), 1},
			{code.S2i, 2, 1},
		},
	},
	{
		"float", `
20.0
`,
		[]code.Instr{
			{code.Push, 20.0, 1},
		},
	},
	{
		"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]code.Instr{
			{code.Otherwise, nil, 2},
			{code.Jnm, 7, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Inc, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]code.Instr{
			{code.Push, int64(1), 2},
			{code.Push, int64(0), 2},
			{code.Icmp, 1, 2},
			{code.Jnm, 6, 2},
			{code.Push, true, 2},
			{code.Jmp, 7, 2},
			{code.Push, false, 2},
			{code.Jnm, 14, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Inc, nil, 3},
			{code.Setmatched, true, 2},
			{code.Jmp, 17, 2},
			{code.Mload, 1, 5},
			{code.Dload, 0, 5},
			{code.Inc, nil, 5},
		},
	},
	{
		"mod",
		`
gauge a
a = 3 % 1
`,
		[]code.Instr{
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Push, int64(3), 2},
			{code.Push, int64(1), 2},
			{code.Imod, nil, 2},
			{code.Iset, nil, 2},
		},
	},
	{
		"del", `
counter a by b
del a["string"]
`,
		[]code.Instr{
			{code.Str, 0, 2},
			{code.Mload, 0, 2},
			{code.Del, 1, 2},
		},
	},
	{
		"del after", `
counter a by b
del a["string"] after 1h
`,
		[]code.Instr{
			{code.Push, time.Hour, 2},
			{code.Str, 0, 2},
			{code.Mload, 0, 2},
			{code.Expire, 1, 2},
		},
	},
	{
		"types", `
gauge i
gauge f
/(\d+)/ {
 i = $1
}
/(\d+\.\d+)/ {
 f = $1
}
`,
		[]code.Instr{
			{code.Match, 0, 3},
			{code.Jnm, 10, 3},
			{code.Setmatched, false, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Push, 0, 4},
			{code.Capref, 1, 4},
			{code.S2i, nil, 4},
			{code.Iset, nil, 4},
			{code.Setmatched, true, 3},
			{code.Match, 1, 6},
			{code.Jnm, 20, 6},
			{code.Setmatched, false, 6},
			{code.Mload, 1, 7},
			{code.Dload, 0, 7},
			{code.Push, 1, 7},
			{code.Capref, 1, 7},
			{code.S2f, nil, 7},
			{code.Fset, nil, 7},
			{code.Setmatched, true, 6},
		},
	},

	{
		"getfilename", `
getfilename()
`,
		[]code.Instr{
			{code.Getfilename, 0, 1},
		},
	},

	{
		"dimensioned counter",
		`counter c by a,b,c
/(\d) (\d) (\d)/ {
  c[$1,$2][$3]++
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 13, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.Push, 0, 2},
			{code.Capref, 2, 2},
			{code.Push, 0, 2},
			{code.Capref, 3, 2},
			{code.Mload, 0, 2},
			{code.Dload, 3, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"string to int",
		`counter c
/(.*)/ {
  c = int($1)
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 10, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2i, nil, 2},
			{code.Iset, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"int to float",
		`counter c
/(\d)/ {
  c = float($1)
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 10, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2f, nil, 2},
			{code.Fset, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"string to float",
		`counter c
/(.*)/ {
  c = float($1)
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 10, 1},
			{code.Setmatched, false, 1},
			{code.Mload, 0, 2},
			{code.Dload, 0, 2},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2f, nil, 2},
			{code.Fset, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"float to string",
		`counter c by a
/(\d+\.\d+)/ {
  c[string($1)] ++
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 11, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2f, nil, 2},
			{code.F2s, nil, 2},
			{code.Mload, 0, 2},
			{code.Dload, 1, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"int to string",
		`counter c by a
/(\d+)/ {
  c[string($1)] ++
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 11, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.S2i, nil, 2},
			{code.I2s, nil, 2},
			{code.Mload, 0, 2},
			{code.Dload, 1, 2},
			{code.Inc, nil, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"nested comparisons",
		`counter foo
/(.*)/ {
  $1 == "foo" || $1 == "bar" {
    foo++
  }
}
`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 31, 1},
			{code.Setmatched, false, 1},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.Str, 0, 2},
			{code.Scmp, 0, 2},
			{code.Jnm, 10, 2},
			{code.Push, true, 2},
			{code.Jmp, 11, 2},
			{code.Push, false, 2},
			{code.Jm, 23, 2},
			{code.Push, 0, 2},
			{code.Capref, 1, 2},
			{code.Str, 1, 2},
			{code.Scmp, 0, 2},
			{code.Jnm, 19, 2},
			{code.Push, true, 2},
			{code.Jmp, 20, 2},
			{code.Push, false, 2},
			{code.Jm, 23, 2},
			{code.Push, false, 2},
			{code.Jmp, 24, 2},
			{code.Push, true, 2},
			{code.Jnm, 30, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Inc, nil, 3},
			{code.Setmatched, true, 2},
			{code.Setmatched, true, 1},
		},
	},
	{
		"string concat", `
counter f by s
/(.*), (.*)/ {
  f[$1 + $2]++
}
`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 12, 2},
			{code.Setmatched, false, 2},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.Push, 0, 3},
			{code.Capref, 2, 3},
			{code.Cat, nil, 3},
			{code.Mload, 0, 3},
			{code.Dload, 1, 3},
			{code.Inc, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"add assign float", `
gauge foo
/(\d+\.\d+)/ {
  foo += $1
}
`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 13, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.S2f, nil, 3},
			{code.Fadd, nil, 3},
			{code.Fset, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"match expression", `
	counter foo
	/(.*)/ {
	  $1 =~ /asdf/ {
	    foo++
	  }
	}`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 13, 2},
			{code.Setmatched, false, 2},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.Smatch, 1, 3},
			{code.Jnm, 12, 3},
			{code.Setmatched, false, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Inc, nil, 4},
			{code.Setmatched, true, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"negative match expression", `
	counter foo
	/(.*)/ {
	  $1 !~ /asdf/ {
	    foo++
	  }
	}`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 14, 2},
			{code.Setmatched, false, 2},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.Smatch, 1, 3},
			{code.Not, nil, 3},
			{code.Jnm, 13, 3},
			{code.Setmatched, false, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Inc, nil, 4},
			{code.Setmatched, true, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"capref used in def", `
/(?P<x>\d+)/ && $x > 5 {
}`,
		[]code.Instr{
			{code.Match, 0, 1},
			{code.Jnm, 14, 1},
			{code.Push, 0, 1},
			{code.Capref, 1, 1},
			{code.S2i, nil, 1},
			{code.Push, int64(5), 1},
			{code.Icmp, 1, 1},
			{code.Jnm, 10, 1},
			{code.Push, true, 1},
			{code.Jmp, 11, 1},
			{code.Push, false, 1},
			{code.Jnm, 14, 1},
			{code.Push, true, 1},
			{code.Jmp, 15, 1},
			{code.Push, false, 1},
			{code.Jnm, 18, 1},
			{code.Setmatched, false, 1},
			{code.Setmatched, true, 1},
		},
	},
	{
		"binop arith type conversion", `
gauge var
/(?P<x>\d+) (\d+\.\d+)/ {
  var = $x + $2
}`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 15, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.S2i, nil, 3},
			{code.I2f, nil, 3},
			{code.Push, 0, 3},
			{code.Capref, 2, 3},
			{code.S2f, nil, 3},
			{code.Fadd, nil, 3},
			{code.Fset, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{
		"binop compare type conversion", `
counter var
/(?P<x>\d+) (\d+\.\d+)/ {
  $x > $2 {
    var++
  }
}`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 22, 2},
			{code.Setmatched, false, 2},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.S2i, nil, 3},
			{code.I2f, nil, 3},
			{code.Push, 0, 3},
			{code.Capref, 2, 3},
			{code.S2f, nil, 3},
			{code.Fcmp, 1, 3},
			{code.Jnm, 14, 3},
			{code.Push, true, 3},
			{code.Jmp, 15, 3},
			{code.Push, false, 3},
			{code.Jnm, 21, 3},
			{code.Setmatched, false, 3},
			{code.Mload, 0, 4},
			{code.Dload, 0, 4},
			{code.Inc, nil, 4},
			{code.Setmatched, true, 3},
			{code.Setmatched, true, 2},
		},
	},
	{"set string", `
text foo
/(.*)/ {
  foo = $1
}
`, []code.Instr{
		{code.Match, 0, 2},
		{code.Jnm, 9, 2},
		{code.Setmatched, false, 2},
		{code.Mload, 0, 3},
		{code.Dload, 0, 3},
		{code.Push, 0, 3},
		{code.Capref, 1, 3},
		{code.Sset, nil, 3},
		{code.Setmatched, true, 2},
	}},
	{
		"concat to text", `
text foo
/(?P<v>.*)/ {
		foo += $v
}`,
		[]code.Instr{
			{code.Match, 0, 2},
			{code.Jnm, 12, 2},
			{code.Setmatched, false, 2},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Mload, 0, 3},
			{code.Dload, 0, 3},
			{code.Push, 0, 3},
			{code.Capref, 1, 3},
			{code.Cat, nil, 3},
			{code.Sset, nil, 3},
			{code.Setmatched, true, 2},
		},
	},
	{"decrement", `
counter i
// {
  i--
}`, []code.Instr{
		{code.Match, 0, 2},
		{code.Jnm, 7, 2},
		{code.Setmatched, false, 2},
		{code.Mload, 0, 3},
		{code.Dload, 0, 3},
		{code.Dec, nil, 3},
		{code.Setmatched, true, 2},
	}},
	{"capref and settime", `
/(\d+)/ {
  settime($1)
}`, []code.Instr{
		{code.Match, 0, 1},
		{code.Jnm, 8, 1},
		{code.Setmatched, false, 1},
		{code.Push, 0, 2},
		{code.Capref, 1, 2},
		{code.S2i, nil, 2},
		{code.Settime, 1, 2},
		{code.Setmatched, true, 1},
	}},
	{"cast to self", `
/(\d+)/ {
settime(int($1))
}`, []code.Instr{
		{code.Match, 0, 1},
		{code.Jnm, 8, 1},
		{code.Setmatched, false, 1},
		{code.Push, 0, 2},
		{code.Capref, 1, 2},
		{code.S2i, nil, 2},
		{code.Settime, 1, 2},
		{code.Setmatched, true, 1},
	}},
	{"stop", `
stop
`, []code.Instr{
		{code.Stop, nil, 1},
	}},
	{"stop inside", `
// {
stop
}
`, []code.Instr{
		{code.Match, 0, 1},
		{code.Jnm, 5, 1},
		{code.Setmatched, false, 1},
		{code.Stop, nil, 2},
		{code.Setmatched, true, 1},
	}},

	{
		"nested decorators",
		`def b {
  def b {
    next
  }
  @b {
    next
  }
}
@b {
}`, nil,
	},
	{"negative numbers in capture groups", `
gauge foo
/(?P<value_ms>-?\d+)/ {
foo += $value_ms / 1000.0
}`, []code.Instr{
		{code.Match, 0, 2},
		{code.Jnm, 16, 2},
		{code.Setmatched, false, 2},
		{code.Mload, 0, 3},
		{code.Dload, 0, 3},
		{code.Mload, 0, 3},
		{code.Dload, 0, 3},
		{code.Push, 0, 3},
		{code.Capref, 1, 3},
		{code.S2i, nil, 3},
		{code.I2f, nil, 3},
		{code.Push, 1000.0, 3},
		{code.Fdiv, nil, 3},
		{code.Fadd, nil, 3},
		{code.Fset, nil, 3},
		{code.Setmatched, true, 2},
	}},
	{"substitution", `
gauge foo
/(\d+,\d)/ {
  foo = int(subst(",", "", $1))
}`, []code.Instr{
		{code.Match, 0, 2},
		{code.Jnm, 13, 2},
		{code.Setmatched, false, 2},
		{code.Mload, 0, 3},
		{code.Dload, 0, 3},
		{code.Str, 0, 3},
		{code.Str, 1, 3},
		{code.Push, 0, 3},
		{code.Capref, 1, 3},
		{code.Subst, 3, 3},
		{code.S2i, nil, 3},
		{code.Iset, nil, 3},
		{code.Setmatched, true, 2},
	}},
	{"const term as pattern", `
const A /n/
A && 1 {
}
`, []code.Instr{
		{code.Match, 0, 0},
		{code.Jnm, 6, 0},
		{code.Push, int64(1), 2},
		{code.Jnm, 6, 0},
		{code.Push, true, 0},
		{code.Jmp, 7, 0},
		{code.Push, false, 0},
		{code.Jnm, 10, 0},
		{code.Setmatched, false, 0},
		{code.Setmatched, true, 0},
	}},
}

func TestCodeGenFromSource(t *testing.T) {
	for _, tc := range testCodeGenPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			ast, err := parser.Parse(tc.name, strings.NewReader(tc.source))
			testutil.FatalIfErr(t, err)
			ast, err = checker.Check(ast, 0, 0)
			if *codegenTestDebug {
				s := parser.Sexp{}
				s.EmitTypes = true
				t.Log("Typed AST:\n" + s.Dump(ast))
			}
			testutil.FatalIfErr(t, err)
			obj, err := codegen.CodeGen(tc.name, ast)
			testutil.FatalIfErr(t, err)

			testutil.ExpectNoDiff(t, tc.prog, obj.Program, testutil.AllowUnexported(code.Instr{}))
		})
	}
}

var testCodeGenASTs = []struct {
	name string
	ast  ast.Node     // partial AST to be converted to bytecode
	prog []code.Instr // expected bytecode
}{
	{
		name: "subst",
		ast: &ast.BuiltinExpr{
			Name: "subst",
			Args: &ast.ExprList{
				Children: []ast.Node{
					&ast.StringLit{
						Text: "old",
					},
					&ast.StringLit{
						Text: "new",
					},
					&ast.StringLit{
						Text: "value",
					},
				},
			},
		},
		prog: []code.Instr{
			{code.Str, 0, 0},
			{code.Str, 1, 0},
			{code.Str, 2, 0},
			{code.Subst, 3, 0},
		},
	},
	{
		name: "regexp subst",
		ast: &ast.BuiltinExpr{
			Name: "subst",
			Args: &ast.ExprList{
				Children: []ast.Node{
					&ast.PatternExpr{
						Pattern: "a+",
						Expr: &ast.PatternLit{
							Pattern: "a+",
						},
					},
					&ast.StringLit{
						Text: "b",
					},
					&ast.StringLit{
						Text: "aaaaaa",
					},
				},
			},
		},
		prog: []code.Instr{
			{code.Str, 0, 0},
			{code.Str, 1, 0},
			{code.Push, 0, 0},
			{code.Rsubst, 3, 0},
		},
	},
}

func TestCodeGenFromAST(t *testing.T) {
	for _, tc := range testCodeGenASTs {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			obj, err := codegen.CodeGen(tc.name, tc.ast)
			testutil.FatalIfErr(t, err)
			testutil.ExpectNoDiff(t, tc.prog, obj.Program, testutil.AllowUnexported(code.Instr{}))
		})
	}
}
