// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/bytecode"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/parser"
)

var testCodeGenPrograms = []struct {
	name   string
	source string
	prog   []bytecode.Instr // expected bytecode
}{
	// Composite literals require too many explicit conversions.
	{"simple line counter",
		"counter line_count\n/$/ { line_count++\n }\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 7},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 7},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Str, 0},
			{bytecode.Strptime, 2},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Str, 0},
			{bytecode.Strptime, 2},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 16},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Inc, 0},
			{bytecode.Mload, 1},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Iset, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, 1},
			{bytecode.Jnm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, -1},
			{bytecode.Jnm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, 0},
			{bytecode.Jnm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, 1},
			{bytecode.Jm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, -1},
			{bytecode.Jm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, 0},
			{bytecode.Jm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"nested cond",
		"counter foo\n" +
			"/(\\d+)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 19},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Push, int64(1)},
			{bytecode.Icmp, 1},
			{bytecode.Jm, 11},
			{bytecode.Push, true},
			{bytecode.Jmp, 12},
			{bytecode.Push, false},
			{bytecode.Jnm, 18},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Setmatched, true}}},
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
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 10},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Mload, 1},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]bytecode.Instr{
			{bytecode.Str, 0},
			{bytecode.Length, 1},
			{bytecode.Push, int64(0)},
			{bytecode.Cmp, 1},
			{bytecode.Jnm, 7},
			{bytecode.Push, true},
			{bytecode.Jmp, 8},
			{bytecode.Push, false},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Setmatched, true}}},
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(7)},
			{bytecode.And, nil},
			{bytecode.Push, int64(15)},
			{bytecode.Xor, nil},
			{bytecode.Push, int64(8)},
			{bytecode.Or, nil},
			{bytecode.Push, int64(16)},
			{bytecode.Neg, nil},
			{bytecode.Push, int64(2)},
			{bytecode.Shl, nil},
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(20)},
			{bytecode.Shr, nil}}},
	{"pow", `
/(\d+) (\d+)/ {
$1 ** $2
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Push, 0},
			{bytecode.Capref, 2},
			{bytecode.S2i, nil},
			{bytecode.Ipow, nil},
			{bytecode.Setmatched, true}}},
	{"indexed expr", `
counter a by b
a["string"]++
`,
		[]bytecode.Instr{
			{bytecode.Str, 0},
			{bytecode.Mload, 0},
			{bytecode.Dload, 1},
			{bytecode.Inc, nil}}},
	{"strtol", `
strtol("deadbeef", 16)
`,
		[]bytecode.Instr{
			{bytecode.Str, 0},
			{bytecode.Push, int64(16)},
			{bytecode.S2i, 2}}},
	{"float", `
20.0
`,
		[]bytecode.Instr{
			{bytecode.Push, 20.0}}},
	{"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]bytecode.Instr{
			{bytecode.Otherwise, nil},
			{bytecode.Jnm, 7},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]bytecode.Instr{
			{bytecode.Push, int64(1)},
			{bytecode.Push, int64(0)},
			{bytecode.Icmp, 1},
			{bytecode.Jnm, 6},
			{bytecode.Push, true},
			{bytecode.Jmp, 7},
			{bytecode.Push, false},
			{bytecode.Jnm, 14},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Jmp, 17},
			{bytecode.Mload, 1},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
		},
	},
	{"mod",
		`
3 % 1
`,
		[]bytecode.Instr{
			{bytecode.Push, int64(3)},
			{bytecode.Push, int64(1)},
			{bytecode.Imod, nil},
		},
	},
	{"del", `
counter a by b
del a["string"]
`,
		[]bytecode.Instr{
			{bytecode.Str, 0},
			{bytecode.Mload, 0},
			{bytecode.Del, 1}},
	},
	{"del after", `
counter a by b
del a["string"] after 1h
`,
		[]bytecode.Instr{
			{bytecode.Push, time.Hour},
			{bytecode.Str, 0},
			{bytecode.Mload, 0},
			{bytecode.Expire, 1}},
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
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 10},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Iset, nil},
			{bytecode.Setmatched, true},
			{bytecode.Match, 1},
			{bytecode.Jnm, 20},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 1},
			{bytecode.Dload, 0},
			{bytecode.Push, 1},
			{bytecode.Capref, 1},
			{bytecode.S2f, nil},
			{bytecode.Fset, nil},
			{bytecode.Setmatched, true},
		},
	},

	{"getfilename", `
getfilename()
`,
		[]bytecode.Instr{
			{bytecode.Getfilename, 0},
		},
	},

	{"dimensioned counter",
		`counter c by a,b,c
/(\d) (\d) (\d)/ {
  c[$1,$2][$3]++
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 19},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.I2s, nil},
			{bytecode.Push, 0},
			{bytecode.Capref, 2},
			{bytecode.S2i, nil},
			{bytecode.I2s, nil},
			{bytecode.Push, 0},
			{bytecode.Capref, 3},
			{bytecode.S2i, nil},
			{bytecode.I2s, nil},
			{bytecode.Mload, 0},
			{bytecode.Dload, 3},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"string to int",
		`counter c
/(.*)/ {
  c = int($1)
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 10},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Iset, nil},
			{bytecode.Setmatched, true}}},
	{"int to float",
		`counter c
/(\d)/ {
  c = float($1)
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.I2f, nil},
			{bytecode.Fset, nil},
			{bytecode.Setmatched, true}}},
	{"string to float",
		`counter c
/(.*)/ {
  c = float($1)
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 10},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2f, nil},
			{bytecode.Fset, nil},
			{bytecode.Setmatched, true}}},
	{"float to string",
		`counter c by a
/(\d+\.\d+)/ {
  c[string($1)] ++
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2f, nil},
			{bytecode.F2s, nil},
			{bytecode.Mload, 0},
			{bytecode.Dload, 1},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"int to string",
		`counter c by a
/(\d+)/ {
  c[string($1)] ++
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 11},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.I2s, nil},
			{bytecode.Mload, 0},
			{bytecode.Dload, 1},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true}}},
	{"nested comparisons",
		`counter foo
/(.*)/ {
  $1 == "foo" || $1 == "bar" {
    foo++
  }
}
`, []bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 31},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Str, 0},
			{bytecode.Scmp, 0},
			{bytecode.Jnm, 10},
			{bytecode.Push, true},
			{bytecode.Jmp, 11},
			{bytecode.Push, false},
			{bytecode.Jm, 23},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Str, 1},
			{bytecode.Scmp, 0},
			{bytecode.Jnm, 19},
			{bytecode.Push, true},
			{bytecode.Jmp, 20},
			{bytecode.Push, false},
			{bytecode.Jm, 23},
			{bytecode.Push, false},
			{bytecode.Jmp, 24},
			{bytecode.Push, true},
			{bytecode.Jnm, 30},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Setmatched, true}}},
	{"string concat", `
counter f by s
/(.*), (.*)/ {
  f[$1 + $2]++
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 12},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Push, 0},
			{bytecode.Capref, 2},
			{bytecode.Cat, nil},
			{bytecode.Mload, 0},
			{bytecode.Dload, 1},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
		}},
	{"add assign float", `
gauge foo
/(\d+\.\d+)/ {
  foo += $1
}
`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2f, nil},
			{bytecode.Fadd, nil},
			{bytecode.Fset, nil},
			{bytecode.Setmatched, true},
		}},
	{"match expression", `
	counter foo
	/(.*)/ {
	  $1 =~ /asdf/ {
	    foo++
	  }
	}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Smatch, 1},
			{bytecode.Jnm, 12},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Setmatched, true},
		}},
	{"negative match expression", `
	counter foo
	/(.*)/ {
	  $1 !~ /asdf/ {
	    foo++
	  }
	}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 14},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Smatch, 1},
			{bytecode.Not, nil},
			{bytecode.Jnm, 13},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Setmatched, true},
		}},
	{"capref used in def", `
/(?P<x>\d+)/ && $x > 5 {
}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 14},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.Push, int64(5)},
			{bytecode.Icmp, 1},
			{bytecode.Jnm, 10},
			{bytecode.Push, true},
			{bytecode.Jmp, 11},
			{bytecode.Push, false},
			{bytecode.Jnm, 14},
			{bytecode.Push, true},
			{bytecode.Jmp, 15},
			{bytecode.Push, false},
			{bytecode.Jnm, 18},
			{bytecode.Setmatched, false},
			{bytecode.Setmatched, true},
		}},
	{"binop arith type conversion", `
gauge var
/(?P<x>\d+) (\d+\.\d+)/ {
  var = $x + $2
}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 15},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.I2f, nil},
			{bytecode.Push, 0},
			{bytecode.Capref, 2},
			{bytecode.S2f, nil},
			{bytecode.Fadd, nil},
			{bytecode.Fset, nil},
			{bytecode.Setmatched, true},
		}},
	{"binop compare type conversion", `
counter var
/(?P<x>\d+) (\d+\.\d+)/ {
  $x > $2 {
    var++
  }
}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 22},
			{bytecode.Setmatched, false},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.S2i, nil},
			{bytecode.I2f, nil},
			{bytecode.Push, 0},
			{bytecode.Capref, 2},
			{bytecode.S2f, nil},
			{bytecode.Fcmp, 1},
			{bytecode.Jnm, 14},
			{bytecode.Push, true},
			{bytecode.Jmp, 15},
			{bytecode.Push, false},
			{bytecode.Jnm, 21},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Inc, nil},
			{bytecode.Setmatched, true},
			{bytecode.Setmatched, true},
		}},
	{"set string", `
text foo
/(.*)/ {
  foo = $1
}
`, []bytecode.Instr{
		{bytecode.Match, 0},
		{bytecode.Jnm, 9},
		{bytecode.Setmatched, false},
		{bytecode.Mload, 0},
		{bytecode.Dload, 0},
		{bytecode.Push, 0},
		{bytecode.Capref, 1},
		{bytecode.Sset, nil},
		{bytecode.Setmatched, true},
	}},
	{"concat to text", `
text foo
/(?P<v>.*)/ {
		foo += $v
}`,
		[]bytecode.Instr{
			{bytecode.Match, 0},
			{bytecode.Jnm, 12},
			{bytecode.Setmatched, false},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Mload, 0},
			{bytecode.Dload, 0},
			{bytecode.Push, 0},
			{bytecode.Capref, 1},
			{bytecode.Cat, nil},
			{bytecode.Sset, nil},
			{bytecode.Setmatched, true},
		}},
	{"decrement", `
counter i
// {
  i--
}`, []bytecode.Instr{
		{bytecode.Match, 0},
		{bytecode.Jnm, 7},
		{bytecode.Setmatched, false},
		{bytecode.Mload, 0},
		{bytecode.Dload, 0},
		{bytecode.Dec, nil},
		{bytecode.Setmatched, true},
	}},
	{"capref and settime", `
/(\d+)/ {
  settime($1)
}`, []bytecode.Instr{
		{bytecode.Match, 0},
		{bytecode.Jnm, 8},
		{bytecode.Setmatched, false},
		{bytecode.Push, 0},
		{bytecode.Capref, 1},
		{bytecode.S2i, nil},
		{bytecode.Settime, 1},
		{bytecode.Setmatched, true},
	}},
	{"cast to self", `
/(\d+)/ {
settime(int($1))
}`, []bytecode.Instr{
		{bytecode.Match, 0},
		{bytecode.Jnm, 8},
		{bytecode.Setmatched, false},
		{bytecode.Push, 0},
		{bytecode.Capref, 1},
		{bytecode.S2i, nil},
		{bytecode.Settime, 1},
		{bytecode.Setmatched, true},
	}},
	{"stop", `
stop
`, []bytecode.Instr{
		{bytecode.Stop, nil},
	}},
	{"stop inside", `
// {
stop
}
`, []bytecode.Instr{
		{bytecode.Match, 0},
		{bytecode.Jnm, 5},
		{bytecode.Setmatched, false},
		{bytecode.Stop, nil},
		{bytecode.Setmatched, true},
	}},
}

func TestCodegen(t *testing.T) {
	for _, tc := range testCodeGenPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ast, err := parser.Parse(tc.name, strings.NewReader(tc.source))
			if err != nil {
				t.Fatalf("Parse error: %s", err)
			}
			ast, err = checker.Check(ast)
			s := parser.Sexp{}
			s.EmitTypes = true
			t.Log("Typed AST:\n" + s.Dump(ast))
			if err != nil {
				t.Fatalf("Check error: %s", err)
			}
			obj, err := CodeGen(tc.name, ast)
			if err != nil {
				t.Fatalf("Codegen error:\n%s", err)
			}

			if diff := testutil.Diff(tc.prog, obj.prog, testutil.AllowUnexported(bytecode.Instr{})); diff != "" {
				t.Error(diff)
				t.Logf("Expected:\n%s\nReceived:\n%s", tc.prog, obj.prog)
			}
		})
	}
}
