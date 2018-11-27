// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"strings"
	"testing"

	go_cmp "github.com/google/go-cmp/cmp"
)

type lexerTest struct {
	name   string
	input  string
	tokens []token
}

var lexerTests = []lexerTest{
	{"empty", "", []token{
		{EOF, "", Position{"empty", 0, 0, 0}}}},
	{"spaces", " \t", []token{
		{EOF, "", Position{"spaces", 0, 2, 2}}}},
	{"newlines", "\n", []token{
		{NL, "\n", Position{"newlines", 1, 0, -1}},
		{EOF, "", Position{"newlines", 1, 0, 0}}}},
	{"comment", "# comment", []token{
		{EOF, "", Position{"comment", 0, 9, 9}}}},
	{"comment not at col 1", "  # comment", []token{
		{EOF, "", Position{"comment not at col 1", 0, 11, 11}}}},
	{"punctuation", "{}()[],", []token{
		{LCURLY, "{", Position{"punctuation", 0, 0, 0}},
		{RCURLY, "}", Position{"punctuation", 0, 1, 1}},
		{LPAREN, "(", Position{"punctuation", 0, 2, 2}},
		{RPAREN, ")", Position{"punctuation", 0, 3, 3}},
		{LSQUARE, "[", Position{"punctuation", 0, 4, 4}},
		{RSQUARE, "]", Position{"punctuation", 0, 5, 5}},
		{COMMA, ",", Position{"punctuation", 0, 6, 6}},
		{EOF, "", Position{"punctuation", 0, 7, 7}}}},
	{"operators", "- + = ++ += < > <= >= == != * / << >> & | ^ ~ ** % || && =~ !~ --", []token{
		{MINUS, "-", Position{"operators", 0, 0, 0}},
		{PLUS, "+", Position{"operators", 0, 2, 2}},
		{ASSIGN, "=", Position{"operators", 0, 4, 4}},
		{INC, "++", Position{"operators", 0, 6, 7}},
		{ADD_ASSIGN, "+=", Position{"operators", 0, 9, 10}},
		{LT, "<", Position{"operators", 0, 12, 12}},
		{GT, ">", Position{"operators", 0, 14, 14}},
		{LE, "<=", Position{"operators", 0, 16, 17}},
		{GE, ">=", Position{"operators", 0, 19, 20}},
		{EQ, "==", Position{"operators", 0, 22, 23}},
		{NE, "!=", Position{"operators", 0, 25, 26}},
		{MUL, "*", Position{"operators", 0, 28, 28}},
		{DIV, "/", Position{"operators", 0, 30, 30}},
		{SHL, "<<", Position{"operators", 0, 32, 33}},
		{SHR, ">>", Position{"operators", 0, 35, 36}},
		{BITAND, "&", Position{"operators", 0, 38, 38}},
		{BITOR, "|", Position{"operators", 0, 40, 40}},
		{XOR, "^", Position{"operators", 0, 42, 42}},
		{NOT, "~", Position{"operators", 0, 44, 44}},
		{POW, "**", Position{"operators", 0, 46, 47}},
		{MOD, "%", Position{"operators", 0, 49, 49}},
		{OR, "||", Position{"operators", 0, 51, 52}},
		{AND, "&&", Position{"operators", 0, 54, 55}},
		{MATCH, "=~", Position{"operators", 0, 57, 58}},
		{NOT_MATCH, "!~", Position{"operators", 0, 60, 61}},
		{DEC, "--", Position{"operators", 0, 63, 64}},
		{EOF, "", Position{"operators", 0, 65, 65}}}},
	{"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\ntimer\notherwise\nelse\ndel\ntext\nafter\nstop\n", []token{
			{COUNTER, "counter", Position{"keywords", 0, 0, 6}},
			{NL, "\n", Position{"keywords", 1, 7, -1}},
			{GAUGE, "gauge", Position{"keywords", 1, 0, 4}},
			{NL, "\n", Position{"keywords", 2, 5, -1}},
			{AS, "as", Position{"keywords", 2, 0, 1}},
			{NL, "\n", Position{"keywords", 3, 2, -1}},
			{BY, "by", Position{"keywords", 3, 0, 1}},
			{NL, "\n", Position{"keywords", 4, 2, -1}},
			{HIDDEN, "hidden", Position{"keywords", 4, 0, 5}},
			{NL, "\n", Position{"keywords", 5, 6, -1}},
			{DEF, "def", Position{"keywords", 5, 0, 2}},
			{NL, "\n", Position{"keywords", 6, 3, -1}},
			{NEXT, "next", Position{"keywords", 6, 0, 3}},
			{NL, "\n", Position{"keywords", 7, 4, -1}},
			{CONST, "const", Position{"keywords", 7, 0, 4}},
			{NL, "\n", Position{"keywords", 8, 5, -1}},
			{TIMER, "timer", Position{"keywords", 8, 0, 4}},
			{NL, "\n", Position{"keywords", 9, 5, -1}},
			{OTHERWISE, "otherwise", Position{"keywords", 9, 0, 8}},
			{NL, "\n", Position{"keywords", 10, 9, -1}},
			{ELSE, "else", Position{"keywords", 10, 0, 3}},
			{NL, "\n", Position{"keywords", 11, 4, -1}},
			{DEL, "del", Position{"keywords", 11, 0, 2}},
			{NL, "\n", Position{"keywords", 12, 3, -1}},
			{TEXT, "text", Position{"keywords", 12, 0, 3}},
			{NL, "\n", Position{"keywords", 13, 4, -1}},
			{AFTER, "after", Position{"keywords", 13, 0, 4}},
			{NL, "\n", Position{"keywords", 14, 5, -1}},
			{STOP, "stop", Position{"keywords", 14, 0, 3}},
			{NL, "\n", Position{"keywords", 15, 4, -1}},
			{EOF, "", Position{"keywords", 15, 0, 0}}}},
	{"builtins",
		"strptime\ntimestamp\ntolower\nlen\nstrtol\nsettime\ngetfilename\nint\nbool\nfloat\nstring\n", []token{
			{BUILTIN, "strptime", Position{"builtins", 0, 0, 7}},
			{NL, "\n", Position{"builtins", 1, 8, -1}},
			{BUILTIN, "timestamp", Position{"builtins", 1, 0, 8}},
			{NL, "\n", Position{"builtins", 2, 9, -1}},
			{BUILTIN, "tolower", Position{"builtins", 2, 0, 6}},
			{NL, "\n", Position{"builtins", 3, 7, -1}},
			{BUILTIN, "len", Position{"builtins", 3, 0, 2}},
			{NL, "\n", Position{"builtins", 4, 3, -1}},
			{BUILTIN, "strtol", Position{"builtins", 4, 0, 5}},
			{NL, "\n", Position{"builtins", 5, 6, -1}},
			{BUILTIN, "settime", Position{"builtins", 5, 0, 6}},
			{NL, "\n", Position{"builtins", 6, 7, -1}},
			{BUILTIN, "getfilename", Position{"builtins", 6, 0, 10}},
			{NL, "\n", Position{"builtins", 7, 11, -1}},
			{BUILTIN, "int", Position{"builtins", 7, 0, 2}},
			{NL, "\n", Position{"builtins", 8, 3, -1}},
			{BUILTIN, "bool", Position{"builtins", 8, 0, 3}},
			{NL, "\n", Position{"builtins", 9, 4, -1}},
			{BUILTIN, "float", Position{"builtins", 9, 0, 4}},
			{NL, "\n", Position{"builtins", 10, 5, -1}},
			{BUILTIN, "string", Position{"builtins", 10, 0, 5}},
			{NL, "\n", Position{"builtins", 11, 6, -1}},
			{EOF, "", Position{"builtins", 11, 0, 0}}}},
	{"numbers", "1 23 3.14 1.61.1 -1 -1.0 1h 0d 3d -1.5h 15m 24h0m0s", []token{
		{INTLITERAL, "1", Position{"numbers", 0, 0, 0}},
		{INTLITERAL, "23", Position{"numbers", 0, 2, 3}},
		{FLOATLITERAL, "3.14", Position{"numbers", 0, 5, 8}},
		{FLOATLITERAL, "1.61", Position{"numbers", 0, 10, 13}},
		{FLOATLITERAL, ".1", Position{"numbers", 0, 14, 15}},
		{INTLITERAL, "-1", Position{"numbers", 0, 17, 18}},
		{FLOATLITERAL, "-1.0", Position{"numbers", 0, 20, 23}},
		{DURATIONLITERAL, "1h", Position{"numbers", 0, 25, 26}},
		{DURATIONLITERAL, "0d", Position{"numbers", 0, 28, 29}},
		{DURATIONLITERAL, "3d", Position{"numbers", 0, 31, 32}},
		{DURATIONLITERAL, "-1.5h", Position{"numbers", 0, 34, 38}},
		{DURATIONLITERAL, "15m", Position{"numbers", 0, 40, 42}},
		{DURATIONLITERAL, "24h0m0s", Position{"numbers", 0, 44, 50}},
		{EOF, "", Position{"numbers", 0, 51, 51}},
	}},
	{"identifier", "a be foo\nquux line_count", []token{
		{ID, "a", Position{"identifier", 0, 0, 0}},
		{ID, "be", Position{"identifier", 0, 2, 3}},
		{ID, "foo", Position{"identifier", 0, 5, 7}},
		{NL, "\n", Position{"identifier", 1, 8, -1}},
		{ID, "quux", Position{"identifier", 1, 0, 3}},
		{ID, "line_count", Position{"identifier", 1, 5, 14}},
		{EOF, "", Position{"identifier", 1, 15, 15}}}},
	{"regex", "/asdf/", []token{
		{DIV, "/", Position{"regex", 0, 0, 0}},
		{REGEX, "asdf", Position{"regex", 0, 1, 4}},
		{DIV, "/", Position{"regex", 0, 5, 5}},
		{EOF, "", Position{"regex", 0, 6, 6}}}},
	{"regex with escape", `/asdf\//`, []token{
		{DIV, "/", Position{"regex with escape", 0, 0, 0}},
		{REGEX, `asdf/`, Position{"regex with escape", 0, 1, 6}},
		{DIV, "/", Position{"regex with escape", 0, 7, 7}},
		{EOF, "", Position{"regex with escape", 0, 8, 8}}}},
	{"regex with escape and special char", `/foo\d\//`, []token{
		{DIV, "/", Position{"regex with escape and special char", 0, 0, 0}},
		{REGEX, `foo\d/`, Position{"regex with escape and special char", 0, 1, 7}},
		{DIV, "/", Position{"regex with escape and special char", 0, 8, 8}},
		{EOF, "", Position{"regex with escape and special char", 0, 9, 9}}}},
	{"capref", "$foo $1", []token{
		{CAPREF_NAMED, "foo", Position{"capref", 0, 0, 3}},
		{CAPREF, "1", Position{"capref", 0, 5, 6}},
		{EOF, "", Position{"capref", 0, 7, 7}}}},
	{"numerical capref", "$1", []token{
		{CAPREF, "1", Position{"numerical capref", 0, 0, 1}},
		{EOF, "", Position{"numerical capref", 0, 2, 2}}}},
	{"capref with trailing punc", "$foo,", []token{
		{CAPREF_NAMED, "foo", Position{"capref with trailing punc", 0, 0, 3}},
		{COMMA, ",", Position{"capref with trailing punc", 0, 4, 4}},
		{EOF, "", Position{"capref with trailing punc", 0, 5, 5}}}},
	{"quoted string", `"asdf"`, []token{
		{STRING, `asdf`, Position{"quoted string", 0, 0, 5}},
		{EOF, "", Position{"quoted string", 0, 6, 6}}}},
	{"escaped quote in quoted string", `"\""`, []token{
		{STRING, `"`, Position{"escaped quote in quoted string", 0, 0, 3}},
		{EOF, "", Position{"escaped quote in quoted string", 0, 4, 4}}}},
	{"decorator", `@foo`, []token{
		{DECO, "foo", Position{"decorator", 0, 0, 3}},
		{EOF, "", Position{"decorator", 0, 4, 4}}}},
	{"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}", []token{
			{DIV, "/", Position{"large program", 0, 0, 0}},
			{REGEX, "(?P<date>[[:digit:]-/ ])", Position{"large program", 0, 1, 25}},
			{DIV, "/", Position{"large program", 0, 26, 26}},
			{LCURLY, "{", Position{"large program", 0, 28, 28}},
			{NL, "\n", Position{"large program", 1, 29, -1}},
			{BUILTIN, "strptime", Position{"large program", 1, 2, 9}},
			{LPAREN, "(", Position{"large program", 1, 10, 10}},
			{CAPREF_NAMED, "date", Position{"large program", 1, 11, 15}},
			{COMMA, ",", Position{"large program", 1, 16, 16}},
			{STRING, "%Y/%m/%d %H:%M:%S", Position{"large program", 1, 18, 36}},
			{RPAREN, ")", Position{"large program", 1, 37, 37}},
			{NL, "\n", Position{"large program", 2, 38, -1}},
			{ID, "foo", Position{"large program", 2, 2, 4}},
			{INC, "++", Position{"large program", 2, 5, 6}},
			{NL, "\n", Position{"large program", 3, 7, -1}},
			{RCURLY, "}", Position{"large program", 3, 0, 0}},
			{EOF, "", Position{"large program", 3, 1, 1}}}},
	{"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo", []token{
			{NL, "\n", Position{"linecount", 3, 12, -1}},
			{ID, "foo", Position{"linecount", 3, 0, 2}},
			{EOF, "", Position{"linecount", 3, 3, 3}}}},
	// errors
	{"unexpected char", "?", []token{
		{INVALID, "Unexpected input: '?'", Position{"unexpected char", 0, 0, 0}},
		{EOF, "", Position{"unexpected char", 0, 1, 1}}}},
	{"unterminated regex", "/foo\n", []token{
		{DIV, "/", Position{"unterminated regex", 0, 0, 0}},
		{INVALID, "Unterminated regular expression: \"/foo\"", Position{"unterminated regex", 0, 1, 3}},
		{EOF, "", Position{"unterminated regex", 0, 4, 4}}}},
	{"unterminated quoted string", "\"foo\n", []token{
		{INVALID, "Unterminated quoted string: \"\\\"foo\"", Position{"unterminated quoted string", 0, 0, 3}},
		{EOF, "", Position{"unterminated quoted string", 0, 4, 4}}}},
}

// collect gathers the emitted items into a slice.
func collect(t *lexerTest) (tokens []token) {
	// Hack to count divs seen for regex tests.
	inRegexSet := false
	l := newLexer(t.name, strings.NewReader(t.input))
	for {
		tok := l.nextToken()
		// Hack to simulate context signal from parser.
		if tok.kind == DIV && (strings.Contains(t.name, "regex") || strings.HasPrefix(t.name, "large program")) && !inRegexSet {
			l.inRegex = true
			inRegexSet = true
		}
		tokens = append(tokens, tok)
		if tok.kind == EOF {
			return
		}
	}
}

func TestLex(t *testing.T) {
	for _, tc := range lexerTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			tokens := collect(&tc)

			if diff := go_cmp.Diff(tc.tokens, tokens, go_cmp.AllowUnexported(token{}, Position{})); diff != "" {
				t.Errorf("-expected +received\n%s", diff)
				t.Logf("received: %v", tokens)
			}
		})
	}
}
