// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/checker"
	"github.com/google/mtail/internal/vm/parser"
)

var testCodeGenPrograms = []struct {
	name   string
	source string
	prog   []Instr // expected bytecode
}{
	// Composite literals require too many explicit conversions.
	{"simple line counter",
		"counter line_count\n/$/ { line_count++\n }\n",
		[]Instr{
			{Match, 0},
			{Jnm, 7},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"count a",
		"counter a_count\n/a$/ { a_count++\n }\n",
		[]Instr{
			{Match, 0},
			{Jnm, 7},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"strptime and capref",
		"counter foo\n" +
			"/(.*)/ { strptime($1, \"2006-01-02T15:04:05\")\n" +
			"foo++\n}\n",
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Str, 0},
			{Strptime, 2},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"strptime and named capref",
		"counter foo\n" +
			"/(?P<date>.*)/ { strptime($date, \"2006-01-02T15:04:05\")\n" +
			"foo++\n }\n",
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Str, 0},
			{Strptime, 2},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"inc by and set",
		"counter foo\ncounter bar\n" +
			"/([0-9]+)/ {\n" +
			"foo += $1\n" +
			"bar = $1\n" +
			"}\n",
		[]Instr{
			{Match, 0},
			{Jnm, 16},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Inc, 0},
			{Mload, 1},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Iset, nil},
			{Setmatched, true}}},
	{"cond expr gt",
		"counter foo\n" +
			"1 > 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, 1},
			{Jnm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond expr lt",
		"counter foo\n" +
			"1 < 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, -1},
			{Jnm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond expr eq",
		"counter foo\n" +
			"1 == 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, 0},
			{Jnm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond expr le",
		"counter foo\n" +
			"1 <= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, 1},
			{Jm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond expr ge",
		"counter foo\n" +
			"1 >= 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, -1},
			{Jm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond expr ne",
		"counter foo\n" +
			"1 != 0 {\n" +
			"  foo++\n" +
			"}\n",
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, 0},
			{Jm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"nested cond",
		"counter foo\n" +
			"/(\\d+)/ {\n" +
			"  $1 <= 1 {\n" +
			"    foo++\n" +
			"  }\n" +
			"}\n",
		[]Instr{
			{Match, 0},
			{Jnm, 19},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Push, int64(1)},
			{Icmp, 1},
			{Jm, 11},
			{Push, true},
			{Jmp, 12},
			{Push, false},
			{Jnm, 18},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Setmatched, true}}},
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
		[]Instr{
			{Match, 0},
			{Jnm, 10},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Mload, 1},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"length",
		"len(\"foo\") > 0 {\n" +
			"}\n",
		[]Instr{
			{Str, 0},
			{Length, 1},
			{Push, int64(0)},
			{Cmp, 1},
			{Jnm, 7},
			{Push, true},
			{Jmp, 8},
			{Push, false},
			{Jnm, 11},
			{Setmatched, false},
			{Setmatched, true}}},
	{"bitwise", `
1 & 7 ^ 15 | 8
~ 16 << 2
1 >> 20
`,
		[]Instr{
			{Push, int64(1)},
			{Push, int64(7)},
			{And, nil},
			{Push, int64(15)},
			{Xor, nil},
			{Push, int64(8)},
			{Or, nil},
			{Push, int64(16)},
			{Neg, nil},
			{Push, int64(2)},
			{Shl, nil},
			{Push, int64(1)},
			{Push, int64(20)},
			{Shr, nil}}},
	{"pow", `
/(\d+) (\d+)/ {
$1 ** $2
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Push, 0},
			{Capref, 2},
			{S2i, nil},
			{Ipow, nil},
			{Setmatched, true}}},
	{"indexed expr", `
counter a by b
a["string"]++
`,
		[]Instr{
			{Str, 0},
			{Mload, 0},
			{Dload, 1},
			{Inc, nil}}},
	{"strtol", `
strtol("deadbeef", 16)
`,
		[]Instr{
			{Str, 0},
			{Push, int64(16)},
			{S2i, 2}}},
	{"float", `
20.0
`,
		[]Instr{
			{Push, 20.0}}},
	{"otherwise", `
counter a
otherwise {
	a++
}
`,
		[]Instr{
			{Otherwise, nil},
			{Jnm, 7},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true}}},
	{"cond else",
		`counter foo
counter bar
1 > 0 {
  foo++
} else {
  bar++
}`,
		[]Instr{
			{Push, int64(1)},
			{Push, int64(0)},
			{Icmp, 1},
			{Jnm, 6},
			{Push, true},
			{Jmp, 7},
			{Push, false},
			{Jnm, 14},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Jmp, 17},
			{Mload, 1},
			{Dload, 0},
			{Inc, nil},
		},
	},
	{"mod",
		`
3 % 1
`,
		[]Instr{
			{Push, int64(3)},
			{Push, int64(1)},
			{Imod, nil},
		},
	},
	{"del", `
counter a by b
del a["string"]
`,
		[]Instr{
			{Str, 0},
			{Mload, 0},
			{Del, 1}},
	},
	{"del after", `
counter a by b
del a["string"] after 1h
`,
		[]Instr{
			{Push, time.Hour},
			{Str, 0},
			{Mload, 0},
			{Expire, 1}},
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
		[]Instr{
			{Match, 0},
			{Jnm, 10},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Iset, nil},
			{Setmatched, true},
			{Match, 1},
			{Jnm, 20},
			{Setmatched, false},
			{Mload, 1},
			{Dload, 0},
			{Push, 1},
			{Capref, 1},
			{S2f, nil},
			{Fset, nil},
			{Setmatched, true},
		},
	},

	{"getfilename", `
getfilename()
`,
		[]Instr{
			{Getfilename, 0},
		},
	},

	{"dimensioned counter",
		`counter c by a,b,c
/(\d) (\d) (\d)/ {
  c[$1,$2][$3]++
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 19},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{I2s, nil},
			{Push, 0},
			{Capref, 2},
			{S2i, nil},
			{I2s, nil},
			{Push, 0},
			{Capref, 3},
			{S2i, nil},
			{I2s, nil},
			{Mload, 0},
			{Dload, 3},
			{Inc, nil},
			{Setmatched, true}}},
	{"string to int",
		`counter c
/(.*)/ {
  c = int($1)
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 10},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Iset, nil},
			{Setmatched, true}}},
	{"int to float",
		`counter c
/(\d)/ {
  c = float($1)
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{I2f, nil},
			{Fset, nil},
			{Setmatched, true}}},
	{"string to float",
		`counter c
/(.*)/ {
  c = float($1)
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 10},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2f, nil},
			{Fset, nil},
			{Setmatched, true}}},
	{"float to string",
		`counter c by a
/(\d+\.\d+)/ {
  c[string($1)] ++
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2f, nil},
			{F2s, nil},
			{Mload, 0},
			{Dload, 1},
			{Inc, nil},
			{Setmatched, true}}},
	{"int to string",
		`counter c by a
/(\d+)/ {
  c[string($1)] ++
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 11},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{I2s, nil},
			{Mload, 0},
			{Dload, 1},
			{Inc, nil},
			{Setmatched, true}}},
	{"nested comparisons",
		`counter foo
/(.*)/ {
  $1 == "foo" || $1 == "bar" {
    foo++
  }
}
`, []Instr{
			{Match, 0},
			{Jnm, 31},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Str, 0},
			{Scmp, 0},
			{Jnm, 10},
			{Push, true},
			{Jmp, 11},
			{Push, false},
			{Jm, 23},
			{Push, 0},
			{Capref, 1},
			{Str, 1},
			{Scmp, 0},
			{Jnm, 19},
			{Push, true},
			{Jmp, 20},
			{Push, false},
			{Jm, 23},
			{Push, false},
			{Jmp, 24},
			{Push, true},
			{Jnm, 30},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Setmatched, true}}},
	{"string concat", `
counter f by s
/(.*), (.*)/ {
  f[$1 + $2]++
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 12},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Push, 0},
			{Capref, 2},
			{Cat, nil},
			{Mload, 0},
			{Dload, 1},
			{Inc, nil},
			{Setmatched, true},
		}},
	{"add assign float", `
gauge foo
/(\d+\.\d+)/ {
  foo += $1
}
`,
		[]Instr{
			{Match, 0},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2f, nil},
			{Fadd, nil},
			{Fset, nil},
			{Setmatched, true},
		}},
	{"match expression", `
	counter foo
	/(.*)/ {
	  $1 =~ /asdf/ {
	    foo++
	  }
	}`,
		[]Instr{
			{Match, 0},
			{Jnm, 13},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Smatch, 1},
			{Jnm, 12},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Setmatched, true},
		}},
	{"negative match expression", `
	counter foo
	/(.*)/ {
	  $1 !~ /asdf/ {
	    foo++
	  }
	}`,
		[]Instr{
			{Match, 0},
			{Jnm, 14},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{Smatch, 1},
			{Not, nil},
			{Jnm, 13},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Setmatched, true},
		}},
	{"capref used in def", `
/(?P<x>\d+)/ && $x > 5 {
}`,
		[]Instr{
			{Match, 0},
			{Jnm, 14},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{Push, int64(5)},
			{Icmp, 1},
			{Jnm, 10},
			{Push, true},
			{Jmp, 11},
			{Push, false},
			{Jnm, 14},
			{Push, true},
			{Jmp, 15},
			{Push, false},
			{Jnm, 18},
			{Setmatched, false},
			{Setmatched, true},
		}},
	{"binop arith type conversion", `
gauge var
/(?P<x>\d+) (\d+\.\d+)/ {
  var = $x + $2
}`,
		[]Instr{
			{Match, 0},
			{Jnm, 15},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{I2f, nil},
			{Push, 0},
			{Capref, 2},
			{S2f, nil},
			{Fadd, nil},
			{Fset, nil},
			{Setmatched, true},
		}},
	{"binop compare type conversion", `
counter var
/(?P<x>\d+) (\d+\.\d+)/ {
  $x > $2 {
    var++
  }
}`,
		[]Instr{
			{Match, 0},
			{Jnm, 22},
			{Setmatched, false},
			{Push, 0},
			{Capref, 1},
			{S2i, nil},
			{I2f, nil},
			{Push, 0},
			{Capref, 2},
			{S2f, nil},
			{Fcmp, 1},
			{Jnm, 14},
			{Push, true},
			{Jmp, 15},
			{Push, false},
			{Jnm, 21},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Inc, nil},
			{Setmatched, true},
			{Setmatched, true},
		}},
	{"set string", `
text foo
/(.*)/ {
  foo = $1
}
`, []Instr{
		{Match, 0},
		{Jnm, 9},
		{Setmatched, false},
		{Mload, 0},
		{Dload, 0},
		{Push, 0},
		{Capref, 1},
		{Sset, nil},
		{Setmatched, true},
	}},
	{"concat to text", `
text foo
/(?P<v>.*)/ {
		foo += $v
}`,
		[]Instr{
			{Match, 0},
			{Jnm, 12},
			{Setmatched, false},
			{Mload, 0},
			{Dload, 0},
			{Mload, 0},
			{Dload, 0},
			{Push, 0},
			{Capref, 1},
			{Cat, nil},
			{Sset, nil},
			{Setmatched, true},
		}},
	{"decrement", `
counter i
// {
  i--
}`, []Instr{
		{Match, 0},
		{Jnm, 7},
		{Setmatched, false},
		{Mload, 0},
		{Dload, 0},
		{Dec, nil},
		{Setmatched, true},
	}},
	{"capref and settime", `
/(\d+)/ {
  settime($1)
}`, []Instr{
		{Match, 0},
		{Jnm, 8},
		{Setmatched, false},
		{Push, 0},
		{Capref, 1},
		{S2i, nil},
		{Settime, 1},
		{Setmatched, true},
	}},
	{"cast to self", `
/(\d+)/ {
settime(int($1))
}`, []Instr{
		{Match, 0},
		{Jnm, 8},
		{Setmatched, false},
		{Push, 0},
		{Capref, 1},
		{S2i, nil},
		{Settime, 1},
		{Setmatched, true},
	}},
	{"stop", `
stop
`, []Instr{
		{Stop, nil},
	}},
	{"stop inside", `
// {
stop
}
`, []Instr{
		{Match, 0},
		{Jnm, 5},
		{Setmatched, false},
		{Stop, nil},
		{Setmatched, true},
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

			if diff := testutil.Diff(tc.prog, obj.prog, testutil.AllowUnexported(Instr{})); diff != "" {
				t.Error(diff)
				t.Logf("Expected:\n%s\nReceived:\n%s", tc.prog, obj.prog)
			}
		})
	}
}
