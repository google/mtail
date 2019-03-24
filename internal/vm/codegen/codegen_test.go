// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package codegen_test

import (
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/code"
	"github.com/google/mtail/internal/vm/codegen"
	"github.com/google/mtail/internal/vm/parser"
)

var testCodeGenPrograms = []struct {
	name   string
	source string
	prog   []code.Instr // expected bytecode
}{
	// Composite literals require too many explicit conversions.
	{"simple line counter",
		"counter line_count\n/$/ { line_count++\n }\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 7},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 7},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Str, 0},
			{code.Strptime, 2},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Str, 0},
			{code.Strptime, 2},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 16},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Inc, 0},
			{code.Mload, 1},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Iset, nil},
			{code.Setmatched, true}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, 1},
			{code.Jnm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, -1},
			{code.Jnm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, 0},
			{code.Jnm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, 1},
			{code.Jm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, -1},
			{code.Jm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, 0},
			{code.Jm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"nested cond",
		"counter foo\n" +
			"/(\\d+)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 19},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Push, int64(1)},
			{code.Icmp, 1},
			{code.Jm, 11},
			{code.Push, true},
			{code.Jmp, 12},
			{code.Push, false},
			{code.Jnm, 18},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Setmatched, true}}},
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
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 10},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Mload, 1},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]code.Instr{
			{code.Str, 0},
			{code.Length, 1},
			{code.Push, int64(0)},
			{code.Cmp, 1},
			{code.Jnm, 7},
			{code.Push, true},
			{code.Jmp, 8},
			{code.Push, false},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Setmatched, true}}},
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(7)},
			{code.And, nil},
			{code.Push, int64(15)},
			{code.Xor, nil},
			{code.Push, int64(8)},
			{code.Or, nil},
			{code.Push, int64(16)},
			{code.Neg, nil},
			{code.Push, int64(2)},
			{code.Shl, nil},
			{code.Push, int64(1)},
			{code.Push, int64(20)},
			{code.Shr, nil}}},
	{"pow", `
/(\d+) (\d+)/ {
$1 ** $2
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Push, 0},
			{code.Capref, 2},
			{code.S2i, nil},
			{code.Ipow, nil},
			{code.Setmatched, true}}},
	{"indexed expr", `
counter a by b
a["string"]++
`,
		[]code.Instr{
			{code.Str, 0},
			{code.Mload, 0},
			{code.Dload, 1},
			{code.Inc, nil}}},
	{"strtol", `
strtol("deadbeef", 16)
`,
		[]code.Instr{
			{code.Str, 0},
			{code.Push, int64(16)},
			{code.S2i, 2}}},
	{"float", `
20.0
`,
		[]code.Instr{
			{code.Push, 20.0}}},
	{"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]code.Instr{
			{code.Otherwise, nil},
			{code.Jnm, 7},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]code.Instr{
			{code.Push, int64(1)},
			{code.Push, int64(0)},
			{code.Icmp, 1},
			{code.Jnm, 6},
			{code.Push, true},
			{code.Jmp, 7},
			{code.Push, false},
			{code.Jnm, 14},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Jmp, 17},
			{code.Mload, 1},
			{code.Dload, 0},
			{code.Inc, nil},
		},
	},
	{"mod",
		`
3 % 1
`,
		[]code.Instr{
			{code.Push, int64(3)},
			{code.Push, int64(1)},
			{code.Imod, nil},
		},
	},
	{"del", `
counter a by b
del a["string"]
`,
		[]code.Instr{
			{code.Str, 0},
			{code.Mload, 0},
			{code.Del, 1}},
	},
	{"del after", `
counter a by b
del a["string"] after 1h
`,
		[]code.Instr{
			{code.Push, time.Hour},
			{code.Str, 0},
			{code.Mload, 0},
			{code.Expire, 1}},
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
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 10},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Iset, nil},
			{code.Setmatched, true},
			{code.Match, 1},
			{code.Jnm, 20},
			{code.Setmatched, false},
			{code.Mload, 1},
			{code.Dload, 0},
			{code.Push, 1},
			{code.Capref, 1},
			{code.S2f, nil},
			{code.Fset, nil},
			{code.Setmatched, true},
		},
	},

	{"getfilename", `
getfilename()
`,
		[]code.Instr{
			{code.Getfilename, 0},
		},
	},

	{"dimensioned counter",
		`counter c by a,b,c
/(\d) (\d) (\d)/ {
  c[$1,$2][$3]++
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 19},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.I2s, nil},
			{code.Push, 0},
			{code.Capref, 2},
			{code.S2i, nil},
			{code.I2s, nil},
			{code.Push, 0},
			{code.Capref, 3},
			{code.S2i, nil},
			{code.I2s, nil},
			{code.Mload, 0},
			{code.Dload, 3},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"string to int",
		`counter c
/(.*)/ {
  c = int($1)
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 10},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Iset, nil},
			{code.Setmatched, true}}},
	{"int to float",
		`counter c
/(\d)/ {
  c = float($1)
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.I2f, nil},
			{code.Fset, nil},
			{code.Setmatched, true}}},
	{"string to float",
		`counter c
/(.*)/ {
  c = float($1)
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 10},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2f, nil},
			{code.Fset, nil},
			{code.Setmatched, true}}},
	{"float to string",
		`counter c by a
/(\d+\.\d+)/ {
  c[string($1)] ++
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2f, nil},
			{code.F2s, nil},
			{code.Mload, 0},
			{code.Dload, 1},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"int to string",
		`counter c by a
/(\d+)/ {
  c[string($1)] ++
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 11},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.I2s, nil},
			{code.Mload, 0},
			{code.Dload, 1},
			{code.Inc, nil},
			{code.Setmatched, true}}},
	{"nested comparisons",
		`counter foo
/(.*)/ {
  $1 == "foo" || $1 == "bar" {
    foo++
  }
}
`, []code.Instr{
			{code.Match, 0},
			{code.Jnm, 31},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Str, 0},
			{code.Scmp, 0},
			{code.Jnm, 10},
			{code.Push, true},
			{code.Jmp, 11},
			{code.Push, false},
			{code.Jm, 23},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Str, 1},
			{code.Scmp, 0},
			{code.Jnm, 19},
			{code.Push, true},
			{code.Jmp, 20},
			{code.Push, false},
			{code.Jm, 23},
			{code.Push, false},
			{code.Jmp, 24},
			{code.Push, true},
			{code.Jnm, 30},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Setmatched, true}}},
	{"string concat", `
counter f by s
/(.*), (.*)/ {
  f[$1 + $2]++
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 12},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Push, 0},
			{code.Capref, 2},
			{code.Cat, nil},
			{code.Mload, 0},
			{code.Dload, 1},
			{code.Inc, nil},
			{code.Setmatched, true},
		}},
	{"add assign float", `
gauge foo
/(\d+\.\d+)/ {
  foo += $1
}
`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2f, nil},
			{code.Fadd, nil},
			{code.Fset, nil},
			{code.Setmatched, true},
		}},
	{"match expression", `
	counter foo
	/(.*)/ {
	  $1 =~ /asdf/ {
	    foo++
	  }
	}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Smatch, 1},
			{code.Jnm, 12},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Setmatched, true},
		}},
	{"negative match expression", `
	counter foo
	/(.*)/ {
	  $1 !~ /asdf/ {
	    foo++
	  }
	}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 14},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Smatch, 1},
			{code.Not, nil},
			{code.Jnm, 13},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Setmatched, true},
		}},
	{"capref used in def", `
/(?P<x>\d+)/ && $x > 5 {
}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 14},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.Push, int64(5)},
			{code.Icmp, 1},
			{code.Jnm, 10},
			{code.Push, true},
			{code.Jmp, 11},
			{code.Push, false},
			{code.Jnm, 14},
			{code.Push, true},
			{code.Jmp, 15},
			{code.Push, false},
			{code.Jnm, 18},
			{code.Setmatched, false},
			{code.Setmatched, true},
		}},
	{"binop arith type conversion", `
gauge var
/(?P<x>\d+) (\d+\.\d+)/ {
  var = $x + $2
}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 15},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.I2f, nil},
			{code.Push, 0},
			{code.Capref, 2},
			{code.S2f, nil},
			{code.Fadd, nil},
			{code.Fset, nil},
			{code.Setmatched, true},
		}},
	{"binop compare type conversion", `
counter var
/(?P<x>\d+) (\d+\.\d+)/ {
  $x > $2 {
    var++
  }
}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 22},
			{code.Setmatched, false},
			{code.Push, 0},
			{code.Capref, 1},
			{code.S2i, nil},
			{code.I2f, nil},
			{code.Push, 0},
			{code.Capref, 2},
			{code.S2f, nil},
			{code.Fcmp, 1},
			{code.Jnm, 14},
			{code.Push, true},
			{code.Jmp, 15},
			{code.Push, false},
			{code.Jnm, 21},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Inc, nil},
			{code.Setmatched, true},
			{code.Setmatched, true},
		}},
	{"set string", `
text foo
/(.*)/ {
  foo = $1
}
`, []code.Instr{
		{code.Match, 0},
		{code.Jnm, 9},
		{code.Setmatched, false},
		{code.Mload, 0},
		{code.Dload, 0},
		{code.Push, 0},
		{code.Capref, 1},
		{code.Sset, nil},
		{code.Setmatched, true},
	}},
	{"concat to text", `
text foo
/(?P<v>.*)/ {
		foo += $v
}`,
		[]code.Instr{
			{code.Match, 0},
			{code.Jnm, 12},
			{code.Setmatched, false},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Mload, 0},
			{code.Dload, 0},
			{code.Push, 0},
			{code.Capref, 1},
			{code.Cat, nil},
			{code.Sset, nil},
			{code.Setmatched, true},
		}},
	{"decrement", `
counter i
// {
  i--
}`, []code.Instr{
		{code.Match, 0},
		{code.Jnm, 7},
		{code.Setmatched, false},
		{code.Mload, 0},
		{code.Dload, 0},
		{code.Dec, nil},
		{code.Setmatched, true},
	}},
	{"capref and settime", `
/(\d+)/ {
  settime($1)
}`, []code.Instr{
		{code.Match, 0},
		{code.Jnm, 8},
		{code.Setmatched, false},
		{code.Push, 0},
		{code.Capref, 1},
		{code.S2i, nil},
		{code.Settime, 1},
		{code.Setmatched, true},
	}},
	{"cast to self", `
/(\d+)/ {
settime(int($1))
}`, []code.Instr{
		{code.Match, 0},
		{code.Jnm, 8},
		{code.Setmatched, false},
		{code.Push, 0},
		{code.Capref, 1},
		{code.S2i, nil},
		{code.Settime, 1},
		{code.Setmatched, true},
	}},
	{"stop", `
stop
`, []code.Instr{
		{code.Stop, nil},
	}},
	{"stop inside", `
// {
stop
}
`, []code.Instr{
		{code.Match, 0},
		{code.Jnm, 5},
		{code.Setmatched, false},
		{code.Stop, nil},
		{code.Setmatched, true},
	}},
}

func TestCodegen(t *testing.T) {
	for _, tc := range testCodeGenPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
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
			obj, err := codegen.CodeGen(tc.name, ast)
			if err != nil {
				t.Fatalf("Codegen error:\n%s", err)
			}

			if diff := testutil.Diff(tc.prog, obj.Program, testutil.AllowUnexported(code.Instr{})); diff != "" {
				t.Error(diff)
				t.Logf("Expected:\n%s\nReceived:\n%s", tc.prog, obj.Program)
			}
		})
	}
}
