// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"flag"
	"strings"
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var parserTestDebug = flag.Bool("parser_test_debug", false, "Turn on parser debug output if set.")

var parserTests = []struct {
	name    string
	program string
}{
	{
		"empty",
		"",
	},

	{
		"newline",
		"\n",
	},

	{
		"declare counter",
		"counter lines_total\n",
	},

	{
		"declare counter string name",
		"counter lines_total as \"line-count\"\n",
	},

	{
		"declare dimensioned counter",
		"counter foo by bar\n",
	},

	{
		"declare dimensioned metric with limit",
		"counter foo by a, b limit 100",
	},

	{
		"declare multi-dimensioned counter",
		"counter foo by bar, baz, quux\n",
	},

	{
		"declare hidden counter",
		"hidden counter foo\n",
	},

	{
		"declare gauge",
		"gauge foo\n",
	},

	{
		"declare timer",
		"timer foo\n",
	},

	{
		"declare text",
		"text stringy\n",
	},

	{
		"declare histogram",
		"histogram foo buckets 0, 1, 2\n",
	},
	{
		"declare histogram float",
		"histogram foo buckets 0, 0.01, 0.1, 1, 10\n",
	},
	{
		"declare histogram by ",
		"histogram foo by code buckets 0, 1, 2\n",
	},
	{
		"declare histogram reversed syntax ",
		"histogram foo buckets 0, 1, 2 by code\n",
	},

	{
		"simple pattern action",
		"/foo/ {}\n",
	},

	{
		"increment counter",
		"counter lines_total\n" +
			"/foo/ {\n" +
			"  lines_total++\n" +
			"}\n",
	},

	{
		"decrement counter",
		`counter i
/foo/ {
  i--
}
`,
	},

	{
		"regex match includes escaped slashes",
		"counter foo\n" +
			"/foo\\// { foo++\n}\n",
	},

	{
		"numeric capture group reference",
		"/(foo)/ {\n" +
			"  $1++\n" +
			"}\n",
	},

	{
		"strptime and capref",
		"/(.*)/ {\n" +
			"strptime($1, \"2006-01-02T15:04:05Z07:00\")\n" +
			" }\n",
	},

	{
		"named capture group reference",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"}\n",
	},

	{
		"nested match conditions",
		"counter foo\n" +
			"counter bar\n" +
			"/match(\\d+)/ {\n" +
			"  foo += $1\n" +
			"  /^bleh (\\S+)/ {\n" +
			"    bar++\n" +
			"    $1++\n" +
			"  }\n" +
			"}\n",
	},

	{
		"nested scope",
		"counter foo\n" +
			"/fo(o)/ {\n" +
			"  $1++\n" +
			"  /bar(xxx)/ {\n" +
			"    $1 += $1\n" +
			"    foo = $1\n" +
			"  }\n" +
			"}\n",
	},

	{
		"comment then code",
		"# %d [%p]\n" +
			"/^(?P<date>\\d+\\/\\d+\\/\\d+ \\d+:\\d+:\\d+) \\[(?P<pid>\\d+)\\] / {\n" +
			"  strptime($1, \"2006/01/02 15:04:05\")\n" +
			"}\n",
	},

	{
		"assignment",
		"counter variable\n" +
			"/(?P<foo>.*)/ {\n" +
			"variable = $foo\n" +
			"}\n",
	},

	{
		"increment operator",
		"counter var\n" +
			"/foo/ {\n" +
			"  var++\n" +
			"}\n",
	},

	{
		"incby operator",
		"counter var\n" +
			"/foo/ {\n  var += 2\n}\n",
	},

	{
		"additive",
		"counter time_total\n" +
			"/(?P<foo>.*)/ {\n" +
			" time_total = timestamp() - time_total\n" +
			"}\n",
	},

	{
		"multiplicative",
		"counter a\n" +
			"counter b\n" +
			"   /foo/ {\n   a = a * b\n" +
			"      a = a ** b\n" +
			"}\n",
	},

	{
		"additive and mem storage",
		"counter time_total\n" +
			"counter variable by foo\n" +
			"/(?P<foo>.*)/ {\n" +
			"  time_total += timestamp() - variable[$foo]\n" +
			"}\n",
	},

	{
		"conditional expressions",
		"counter foo\n" +
			"/(?P<foo>.*)/ {\n" +
			"  $foo > 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo >= 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo < 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo <= 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo == 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"  $foo != 0 {\n" +
			"    foo += $foo\n" +
			"  }\n" +
			"}\n",
	},

	{
		"decorator definition and invocation",
		"def foo { next\n }\n" +
			"@foo { }\n",
	},

	{
		"const regex",
		"const X /foo/\n" +
			"/foo / + X + / bar/ {\n" +
			"}\n",
	},

	{
		"multiline regex starting with newline",
		"const FOO\n" +
			"/some regex here/ +\n" +
			"/plus some other things/",
	},

	{
		"multiline regex",
		"/foo / +\n" +
			"/barrr/ {\n" +
			"}\n",
	},

	{
		"len",
		"/(?P<foo>foo)/ {\n" +
			"len($foo) > 0 {\n" +
			"}\n" +
			"}\n",
	},

	{
		"def and next",
		"def foobar {/(?P<date>.*)/ {" +
			"  next" +
			"}" +
			"}",
	},

	{
		"const",
		`const IP /\d+(\.\d+){3}/`,
	},

	{
		"bitwise",
		`gauge a
/foo(\d)/ {
  a = $1 & 7
  a = $1 | 8
  a = $1 << 4
  a = $1 >> 20
  a = $1 ^ 15
  a = ~ 1
}`,
	},

	{
		"logical",
		`0 || 1 && 0 {
}
`,
	},

	{
		"floats",
		`gauge foo
/foo/ {
foo = 3.14
}`,
	},

	{
		"simple otherwise action",
		"otherwise {}\n",
	},

	{
		"pattern action then otherwise action",
		`counter lines_total by type
		/foo/ {
			lines_total["foo"]++
		}
		otherwise {
			lines_total["misc"] += 10
		}`,
	},

	{
		"simple else clause",
		"/foo/ {} else {}",
	},

	{
		"nested else clause",
		"/foo/ { / bar/ {}  } else { /quux/ {} else {} }",
	},

	{
		"mod operator",
		`gauge a
/foo/ {
  a = 3 % 1
}`,
	},

	{
		"delete",
		`counter foo by bar
/foo/ {
  del foo[$1]
}`,
	},

	{
		"delete after",
		`counter foo by bar
/foo/ {
  del foo[$1] after 168h
}`,
	},

	{"getfilename", `
getfilename()
`},

	{"indexed expression arg list", `
counter foo by a,b
/(\d) (\d+)/ {
  foo[$1,$2]++
}`},

	{"paren expr", `
(0) || (1 && 3) {
}`},

	{"regex cond expr", `
/(\d)/ && 1 {
}
`},

	{"concat expr 1", `
const X /foo/
/bar/ + X {
}`},
	{"concat expr 2", `
const X /foo/
X {
}`},

	{"match expression 1", `
$foo =~ /bar/ {
}
$foo !~ /bar/ {
}
`},
	{"match expression 2", `
$foo =~ /bar/ + X {
}`},
	{"match expression 3", `
const X /foo/
$foo =~ X {
}`},

	{"capref used in def", `
/(?P<x>.*)/ && $x > 0 {
}`},

	{"match expr 4", `
/(?P<foo>.{6}) (?P<bar>.*)/ {
  $foo =~ $bar {
  }
}`},

	{"stop", `
// {
  stop
}`},

	{"substitution", `
/(\d,\d)/ {
  subst(",", ",", $1)
}`},

	{"pattern in arg expr list", `
	/(\d,\d)/ {
	    subst(/,/, "", $1)
	}`},
}

func TestParserRoundTrip(t *testing.T) {
	if *parserTestDebug {
		mtailDebug = 3
	}
	for _, tc := range parserTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			p := newParser(tc.name, strings.NewReader(tc.program))
			r := mtailParse(p)

			if r != 0 || p.root == nil || len(p.errors) > 0 {
				t.Error("1st pass parse errors:\n")
				for _, e := range p.errors {
					t.Errorf("\t%s\n", e)
				}
				t.Fatal()
			}

			if *parserTestDebug {
				s := Sexp{}
				t.Log("AST:\n" + s.Dump(p.root))
			}

			u := Unparser{}
			output := u.Unparse(p.root)

			p2 := newParser(tc.name+" 2", strings.NewReader(output))
			r = mtailParse(p2)
			if r != 0 || p2.root == nil || len(p2.errors) > 0 {
				t.Errorf("2nd pass parse errors:\n")
				for _, e := range p2.errors {
					t.Errorf("\t%s\n", e)
				}
				t.Logf("2nd pass input was:\n%s", output)
				t.Logf("2nd pass diff:\n%s", testutil.Diff(tc.program, output))
				t.Fatal()
			}

			u = Unparser{}
			output2 := u.Unparse(p2.root)

			testutil.ExpectNoDiff(t, output2, output)
		})
	}
}

type parserInvalidProgram struct {
	name    string
	program string
	errors  []string
}

var parserInvalidPrograms = []parserInvalidProgram{
	{
		"unknown character",
		"?\n",
		[]string{"unknown character:1:1: Unexpected input: '?'"},
	},

	{
		"unterminated regex",
		"/foo\n",
		[]string{
			"unterminated regex:1:2-4: Unterminated regular expression: \"/foo\"",
			"unterminated regex:1:2-4: syntax error: unexpected end of file, expecting '/' to end regex",
		},
	},

	{
		"unterminated string",
		" \"foo }\n",
		[]string{"unterminated string:1:2-7: Unterminated quoted string: \"\\\"foo }\""},
	},

	{
		"unterminated const regex",
		"const X /(?P<foo>",
		[]string{
			"unterminated const regex:1:10-17: Unterminated regular expression: \"/(?P<foo>\"",
			"unterminated const regex:1:10-17: syntax error: unexpected end of file, expecting '/' to end regex",
		},
	},

	{
		"unbalanced {",
		"/foo/ {\n",
		[]string{"unbalanced {:2:1: syntax error: unexpected end of file, expecting '}' to end block"},
	},
	{
		"unbalanced else {",
		"/foo/ { } else {\n",
		[]string{"unbalanced else {:2:1: syntax error: unexpected end of file, expecting '}' to end block"},
	},
	{
		"unbalanced otherwise {",
		"otherwise {\n",
		[]string{"unbalanced otherwise {:2:1: syntax error: unexpected end of file, expecting '}' to end block"},
	},

	{
		"index of non-terminal 1",
		`// {
	foo++[$1]++
	}`,
		[]string{"index of non-terminal 1:2:7: syntax error: unexpected indexing of an expression"},
	},
	{
		"index of non-terminal 2",
		`// {
	0[$1]++
	}`,
		[]string{"index of non-terminal 2:2:3: syntax error: unexpected indexing of an expression"},
	},
	{
		"index of pattern",
		`/foo/[0]
`,
		[]string{"index of pattern:1:6: syntax error: unexpected indexing of an expression"},
	},

	{
		"statement with no effect",
		`/(\d)foo/ {
 timestamp() - $1
}`,
		[]string{"statement with no effect:3:18: syntax error: statement with no effect, missing an assignment, `+' concatenation, or `{}' block?"},
	},

	{
		"pattern without block",
		`/(?P<a>.)/
`,
		[]string{"pattern without block:2:11: syntax error: statement with no effect, missing an assignment, `+' concatenation, or `{}' block?"},
	},

	{
		"paired pattern without block",
		`/(?P<a>.)/
	/(?P<b>.)/ {}
	`,
		[]string{"paired pattern without block:2:11: syntax error: statement with no effect, missing an assignment, `+' concatenation, or `{}' block?"},
	},

	{
		"dimensioned limit per dimension",
		"counter foo by a limit 10, b",
		[]string{"dimensioned limit per dimension:1:26: syntax error: unexpected COMMA"},
	},
}

func TestParseInvalidPrograms(t *testing.T) {
	if *parserTestDebug {
		mtailDebug = 3
	}
	for _, tc := range parserInvalidPrograms {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			p := newParser(tc.name, strings.NewReader(tc.program))
			mtailParse(p)

			testutil.ExpectNoDiff(t,
				strings.Join(tc.errors, "\n"),             // want
				strings.TrimRight(p.errors.Error(), "\n")) // got
			if p.errors.Error() == "no errors" && *parserTestDebug {
				s := Sexp{}
				t.Log("AST:\n" + s.Dump(p.root))
			}
		})
	}
}

var parsePositionTests = []struct {
	name      string
	program   string
	positions []*position.Position
}{
	{
		"empty",
		"",
		nil,
	},
	{
		"variable",
		`counter foo`,
		[]*position.Position{{"variable", 0, 8, 10}},
	},
	{
		"pattern",
		`const ID /foo/`,
		[]*position.Position{{"pattern", 0, 9, 13}},
	},
	{
		"multiline regex",
		"const ID\n" +
			"/foo/ +\n" +
			"/bar/",
		// TODO: Update position for the first token to `1, 0, 4` when position tracking is fixed
		[]*position.Position{{"multiline regex", 1, 4, 4}, {"multiline regex", 2, 0, 4}},
	},
}

func TestParsePositionTests(t *testing.T) {
	for _, tc := range parsePositionTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			// Not t.Parallel() because the parser is not reentrant, and mtailDebug is a global.
			root, err := Parse(tc.name, strings.NewReader(tc.program))
			testutil.FatalIfErr(t, err)
			p := &positionCollector{}
			ast.Walk(p, root)
			testutil.ExpectNoDiff(t, tc.positions, p.positions, testutil.AllowUnexported(position.Position{}))
		})
	}
}

type positionCollector struct {
	positions []*position.Position
}

func (p *positionCollector) VisitBefore(node ast.Node) (ast.Visitor, ast.Node) {
	switch n := node.(type) {
	case *ast.VarDecl, *ast.PatternLit:
		p.positions = append(p.positions, n.Pos())
	}
	return p, node
}

func (p *positionCollector) VisitAfter(node ast.Node) ast.Node {
	return node
}
