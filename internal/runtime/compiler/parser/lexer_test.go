// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"strings"
	"testing"

	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
	"github.com/jaqx0r/mtail/internal/testutil"
)

type lexerTest struct {
	name   string
	input  string
	tokens []Token
}

var lexerTests = []lexerTest{
	{"empty", "", []Token{
		{EOF, "", position.Position{"empty", 0, 0, 0}},
	}},
	{"spaces", " \t", []Token{
		{EOF, "", position.Position{"spaces", 0, 2, 2}},
	}},
	{"newlines", "\n", []Token{
		{NL, "\n", position.Position{"newlines", 1, 0, -1}},
		{EOF, "", position.Position{"newlines", 1, 0, 0}},
	}},
	{"comment", "# comment", []Token{
		{EOF, "", position.Position{"comment", 0, 9, 9}},
	}},
	{"comment not at col 1", "  # comment", []Token{
		{EOF, "", position.Position{"comment not at col 1", 0, 11, 11}},
	}},
	{"punctuation", "{}()[],", []Token{
		{LCURLY, "{", position.Position{"punctuation", 0, 0, 0}},
		{RCURLY, "}", position.Position{"punctuation", 0, 1, 1}},
		{LPAREN, "(", position.Position{"punctuation", 0, 2, 2}},
		{RPAREN, ")", position.Position{"punctuation", 0, 3, 3}},
		{LSQUARE, "[", position.Position{"punctuation", 0, 4, 4}},
		{RSQUARE, "]", position.Position{"punctuation", 0, 5, 5}},
		{COMMA, ",", position.Position{"punctuation", 0, 6, 6}},
		{EOF, "", position.Position{"punctuation", 0, 7, 7}},
	}},
	{"operators", "- + = ++ += < > <= >= == != * / << >> & | ^ ~ ** % || && =~ !~ --", []Token{
		{MINUS, "-", position.Position{"operators", 0, 0, 0}},
		{PLUS, "+", position.Position{"operators", 0, 2, 2}},
		{ASSIGN, "=", position.Position{"operators", 0, 4, 4}},
		{INC, "++", position.Position{"operators", 0, 6, 7}},
		{ADD_ASSIGN, "+=", position.Position{"operators", 0, 9, 10}},
		{LT, "<", position.Position{"operators", 0, 12, 12}},
		{GT, ">", position.Position{"operators", 0, 14, 14}},
		{LE, "<=", position.Position{"operators", 0, 16, 17}},
		{GE, ">=", position.Position{"operators", 0, 19, 20}},
		{EQ, "==", position.Position{"operators", 0, 22, 23}},
		{NE, "!=", position.Position{"operators", 0, 25, 26}},
		{MUL, "*", position.Position{"operators", 0, 28, 28}},
		{DIV, "/", position.Position{"operators", 0, 30, 30}},
		{SHL, "<<", position.Position{"operators", 0, 32, 33}},
		{SHR, ">>", position.Position{"operators", 0, 35, 36}},
		{BITAND, "&", position.Position{"operators", 0, 38, 38}},
		{BITOR, "|", position.Position{"operators", 0, 40, 40}},
		{XOR, "^", position.Position{"operators", 0, 42, 42}},
		{NOT, "~", position.Position{"operators", 0, 44, 44}},
		{POW, "**", position.Position{"operators", 0, 46, 47}},
		{MOD, "%", position.Position{"operators", 0, 49, 49}},
		{OR, "||", position.Position{"operators", 0, 51, 52}},
		{AND, "&&", position.Position{"operators", 0, 54, 55}},
		{MATCH, "=~", position.Position{"operators", 0, 57, 58}},
		{NOT_MATCH, "!~", position.Position{"operators", 0, 60, 61}},
		{DEC, "--", position.Position{"operators", 0, 63, 64}},
		{EOF, "", position.Position{"operators", 0, 65, 65}},
	}},
	{
		"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\ntimer\notherwise\nelse\ndel\ntext\nafter\nstop\nhistogram\nbuckets\n",
		[]Token{
			{COUNTER, "counter", position.Position{"keywords", 0, 0, 6}},
			{NL, "\n", position.Position{"keywords", 1, 7, -1}},
			{GAUGE, "gauge", position.Position{"keywords", 1, 0, 4}},
			{NL, "\n", position.Position{"keywords", 2, 5, -1}},
			{AS, "as", position.Position{"keywords", 2, 0, 1}},
			{NL, "\n", position.Position{"keywords", 3, 2, -1}},
			{BY, "by", position.Position{"keywords", 3, 0, 1}},
			{NL, "\n", position.Position{"keywords", 4, 2, -1}},
			{HIDDEN, "hidden", position.Position{"keywords", 4, 0, 5}},
			{NL, "\n", position.Position{"keywords", 5, 6, -1}},
			{DEF, "def", position.Position{"keywords", 5, 0, 2}},
			{NL, "\n", position.Position{"keywords", 6, 3, -1}},
			{NEXT, "next", position.Position{"keywords", 6, 0, 3}},
			{NL, "\n", position.Position{"keywords", 7, 4, -1}},
			{CONST, "const", position.Position{"keywords", 7, 0, 4}},
			{NL, "\n", position.Position{"keywords", 8, 5, -1}},
			{TIMER, "timer", position.Position{"keywords", 8, 0, 4}},
			{NL, "\n", position.Position{"keywords", 9, 5, -1}},
			{OTHERWISE, "otherwise", position.Position{"keywords", 9, 0, 8}},
			{NL, "\n", position.Position{"keywords", 10, 9, -1}},
			{ELSE, "else", position.Position{"keywords", 10, 0, 3}},
			{NL, "\n", position.Position{"keywords", 11, 4, -1}},
			{DEL, "del", position.Position{"keywords", 11, 0, 2}},
			{NL, "\n", position.Position{"keywords", 12, 3, -1}},
			{TEXT, "text", position.Position{"keywords", 12, 0, 3}},
			{NL, "\n", position.Position{"keywords", 13, 4, -1}},
			{AFTER, "after", position.Position{"keywords", 13, 0, 4}},
			{NL, "\n", position.Position{"keywords", 14, 5, -1}},
			{STOP, "stop", position.Position{"keywords", 14, 0, 3}},
			{NL, "\n", position.Position{"keywords", 15, 4, -1}},
			{HISTOGRAM, "histogram", position.Position{"keywords", 15, 0, 8}},
			{NL, "\n", position.Position{"keywords", 16, 9, -1}},
			{BUCKETS, "buckets", position.Position{"keywords", 16, 0, 6}},
			{NL, "\n", position.Position{"keywords", 17, 7, -1}},
			{EOF, "", position.Position{"keywords", 17, 0, 0}},
		},
	},
	{
		"builtins",
		"strptime\ntimestamp\ntolower\nlen\nstrtol\nsettime\ngetfilename\nint\nbool\nfloat\nstring\nsubst\n",
		[]Token{
			{BUILTIN, "strptime", position.Position{"builtins", 0, 0, 7}},
			{NL, "\n", position.Position{"builtins", 1, 8, -1}},
			{BUILTIN, "timestamp", position.Position{"builtins", 1, 0, 8}},
			{NL, "\n", position.Position{"builtins", 2, 9, -1}},
			{BUILTIN, "tolower", position.Position{"builtins", 2, 0, 6}},
			{NL, "\n", position.Position{"builtins", 3, 7, -1}},
			{BUILTIN, "len", position.Position{"builtins", 3, 0, 2}},
			{NL, "\n", position.Position{"builtins", 4, 3, -1}},
			{BUILTIN, "strtol", position.Position{"builtins", 4, 0, 5}},
			{NL, "\n", position.Position{"builtins", 5, 6, -1}},
			{BUILTIN, "settime", position.Position{"builtins", 5, 0, 6}},
			{NL, "\n", position.Position{"builtins", 6, 7, -1}},
			{BUILTIN, "getfilename", position.Position{"builtins", 6, 0, 10}},
			{NL, "\n", position.Position{"builtins", 7, 11, -1}},
			{BUILTIN, "int", position.Position{"builtins", 7, 0, 2}},
			{NL, "\n", position.Position{"builtins", 8, 3, -1}},
			{BUILTIN, "bool", position.Position{"builtins", 8, 0, 3}},
			{NL, "\n", position.Position{"builtins", 9, 4, -1}},
			{BUILTIN, "float", position.Position{"builtins", 9, 0, 4}},
			{NL, "\n", position.Position{"builtins", 10, 5, -1}},
			{BUILTIN, "string", position.Position{"builtins", 10, 0, 5}},
			{NL, "\n", position.Position{"builtins", 11, 6, -1}},
			{BUILTIN, "subst", position.Position{"builtins", 11, 0, 4}},
			{NL, "\n", position.Position{"builtins", 12, 5, -1}},
			{EOF, "", position.Position{"builtins", 12, 0, 0}},
		},
	},
	{"numbers", "1 23 3.14 1.61.1 -1 -1.0 1h 0d 3d -1.5h 15m 24h0m0s 1e3 1e-3 .11 123.456e7", []Token{
		{INTLITERAL, "1", position.Position{"numbers", 0, 0, 0}},
		{INTLITERAL, "23", position.Position{"numbers", 0, 2, 3}},
		{FLOATLITERAL, "3.14", position.Position{"numbers", 0, 5, 8}},
		{FLOATLITERAL, "1.61", position.Position{"numbers", 0, 10, 13}},
		{FLOATLITERAL, ".1", position.Position{"numbers", 0, 14, 15}},
		{INTLITERAL, "-1", position.Position{"numbers", 0, 17, 18}},
		{FLOATLITERAL, "-1.0", position.Position{"numbers", 0, 20, 23}},
		{DURATIONLITERAL, "1h", position.Position{"numbers", 0, 25, 26}},
		{DURATIONLITERAL, "0d", position.Position{"numbers", 0, 28, 29}},
		{DURATIONLITERAL, "3d", position.Position{"numbers", 0, 31, 32}},
		{DURATIONLITERAL, "-1.5h", position.Position{"numbers", 0, 34, 38}},
		{DURATIONLITERAL, "15m", position.Position{"numbers", 0, 40, 42}},
		{DURATIONLITERAL, "24h0m0s", position.Position{"numbers", 0, 44, 50}},
		{FLOATLITERAL, "1e3", position.Position{"numbers", 0, 52, 54}},
		{FLOATLITERAL, "1e-3", position.Position{"numbers", 0, 56, 59}},
		{FLOATLITERAL, ".11", position.Position{"numbers", 0, 61, 63}},
		{FLOATLITERAL, "123.456e7", position.Position{"numbers", 0, 65, 73}},
		{EOF, "", position.Position{"numbers", 0, 74, 74}},
	}},
	{"identifier", "a be foo\nquux lines_total", []Token{
		{ID, "a", position.Position{"identifier", 0, 0, 0}},
		{ID, "be", position.Position{"identifier", 0, 2, 3}},
		{ID, "foo", position.Position{"identifier", 0, 5, 7}},
		{NL, "\n", position.Position{"identifier", 1, 8, -1}},
		{ID, "quux", position.Position{"identifier", 1, 0, 3}},
		{ID, "lines_total", position.Position{"identifier", 1, 5, 15}},
		{EOF, "", position.Position{"identifier", 1, 16, 16}},
	}},
	{"regex", "/asdf/", []Token{
		{DIV, "/", position.Position{"regex", 0, 0, 0}},
		{REGEX, "asdf", position.Position{"regex", 0, 1, 4}},
		{DIV, "/", position.Position{"regex", 0, 5, 5}},
		{EOF, "", position.Position{"regex", 0, 6, 6}},
	}},
	{"regex with escape", `/asdf\//`, []Token{
		{DIV, "/", position.Position{"regex with escape", 0, 0, 0}},
		{REGEX, `asdf/`, position.Position{"regex with escape", 0, 1, 6}},
		{DIV, "/", position.Position{"regex with escape", 0, 7, 7}},
		{EOF, "", position.Position{"regex with escape", 0, 8, 8}},
	}},
	{"regex with escape and special char", `/foo\d\//`, []Token{
		{DIV, "/", position.Position{"regex with escape and special char", 0, 0, 0}},
		{REGEX, `foo\d/`, position.Position{"regex with escape and special char", 0, 1, 7}},
		{DIV, "/", position.Position{"regex with escape and special char", 0, 8, 8}},
		{EOF, "", position.Position{"regex with escape and special char", 0, 9, 9}},
	}},
	{"capref", "$foo $1", []Token{
		{CAPREF_NAMED, "foo", position.Position{"capref", 0, 0, 3}},
		{CAPREF, "1", position.Position{"capref", 0, 5, 6}},
		{EOF, "", position.Position{"capref", 0, 7, 7}},
	}},
	{"numerical capref", "$1", []Token{
		{CAPREF, "1", position.Position{"numerical capref", 0, 0, 1}},
		{EOF, "", position.Position{"numerical capref", 0, 2, 2}},
	}},
	{"capref with trailing punc", "$foo,", []Token{
		{CAPREF_NAMED, "foo", position.Position{"capref with trailing punc", 0, 0, 3}},
		{COMMA, ",", position.Position{"capref with trailing punc", 0, 4, 4}},
		{EOF, "", position.Position{"capref with trailing punc", 0, 5, 5}},
	}},
	{"quoted string", `"asdf"`, []Token{
		{STRING, `asdf`, position.Position{"quoted string", 0, 0, 5}},
		{EOF, "", position.Position{"quoted string", 0, 6, 6}},
	}},
	{"escaped quote in quoted string", `"\""`, []Token{
		{STRING, `"`, position.Position{"escaped quote in quoted string", 0, 0, 3}},
		{EOF, "", position.Position{"escaped quote in quoted string", 0, 4, 4}},
	}},
	{"decorator", `@foo`, []Token{
		{DECO, "foo", position.Position{"decorator", 0, 0, 3}},
		{EOF, "", position.Position{"decorator", 0, 4, 4}},
	}},
	{
		"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}",
		[]Token{
			{DIV, "/", position.Position{"large program", 0, 0, 0}},
			{REGEX, "(?P<date>[[:digit:]-/ ])", position.Position{"large program", 0, 1, 25}},
			{DIV, "/", position.Position{"large program", 0, 26, 26}},
			{LCURLY, "{", position.Position{"large program", 0, 28, 28}},
			{NL, "\n", position.Position{"large program", 1, 29, -1}},
			{BUILTIN, "strptime", position.Position{"large program", 1, 2, 9}},
			{LPAREN, "(", position.Position{"large program", 1, 10, 10}},
			{CAPREF_NAMED, "date", position.Position{"large program", 1, 11, 15}},
			{COMMA, ",", position.Position{"large program", 1, 16, 16}},
			{STRING, "%Y/%m/%d %H:%M:%S", position.Position{"large program", 1, 18, 36}},
			{RPAREN, ")", position.Position{"large program", 1, 37, 37}},
			{NL, "\n", position.Position{"large program", 2, 38, -1}},
			{ID, "foo", position.Position{"large program", 2, 2, 4}},
			{INC, "++", position.Position{"large program", 2, 5, 6}},
			{NL, "\n", position.Position{"large program", 3, 7, -1}},
			{RCURLY, "}", position.Position{"large program", 3, 0, 0}},
			{EOF, "", position.Position{"large program", 3, 1, 1}},
		},
	},
	{
		"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo",
		[]Token{
			{NL, "\n", position.Position{"linecount", 3, 12, -1}},
			{ID, "foo", position.Position{"linecount", 3, 0, 2}},
			{EOF, "", position.Position{"linecount", 3, 3, 3}},
		},
	},
	// errors
	{"unexpected char", "?", []Token{
		{INVALID, "Unexpected input: '?'", position.Position{"unexpected char", 0, 0, 0}},
		{EOF, "", position.Position{"unexpected char", 0, 1, 1}},
	}},
	{"unterminated regex", "/foo\n", []Token{
		{DIV, "/", position.Position{"unterminated regex", 0, 0, 0}},
		{INVALID, "Unterminated regular expression: \"/foo\"", position.Position{"unterminated regex", 0, 1, 3}},
		{EOF, "", position.Position{"unterminated regex", 0, 4, 4}},
	}},
	{"unterminated quoted string", "\"foo\n", []Token{
		{INVALID, "Unterminated quoted string: \"\\\"foo\"", position.Position{"unterminated quoted string", 0, 0, 3}},
		{EOF, "", position.Position{"unterminated quoted string", 0, 4, 4}},
	}},
}

// collect gathers the emitted items into a slice.
func collect(t *lexerTest) (tokens []Token) {
	// Hack to count divs seen for regex tests.
	inRegexSet := false
	l := NewLexer(t.name, strings.NewReader(t.input))
	for {
		tok := l.NextToken()
		// Hack to simulate context signal from parser.
		if tok.Kind == DIV && (strings.Contains(t.name, "regex") || strings.HasPrefix(t.name, "large program")) && !inRegexSet {
			l.InRegex = true
			inRegexSet = true
		}
		tokens = append(tokens, tok)
		if tok.Kind == EOF {
			return
		}
	}
}

func TestLex(t *testing.T) {
	for _, tc := range lexerTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			tokens := collect(&tc)

			testutil.ExpectNoDiff(t, tc.tokens, tokens, testutil.AllowUnexported(Token{}, position.Position{}))
		})
	}
}
