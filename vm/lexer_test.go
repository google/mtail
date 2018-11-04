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
		{EOF, "", position{"empty", 0, 0, 0}}}},
	{"spaces", " \t", []token{
		{EOF, "", position{"spaces", 0, 2, 2}}}},
	{"newlines", "\n", []token{
		{NL, "\n", position{"newlines", 1, 0, -1}},
		{EOF, "", position{"newlines", 1, 0, 0}}}},
	{"comment", "# comment", []token{
		{EOF, "", position{"comment", 0, 9, 9}}}},
	{"comment not at col 1", "  # comment", []token{
		{EOF, "", position{"comment not at col 1", 0, 11, 11}}}},
	{"punctuation", "{}()[],", []token{
		{LCURLY, "{", position{"punctuation", 0, 0, 0}},
		{RCURLY, "}", position{"punctuation", 0, 1, 1}},
		{LPAREN, "(", position{"punctuation", 0, 2, 2}},
		{RPAREN, ")", position{"punctuation", 0, 3, 3}},
		{LSQUARE, "[", position{"punctuation", 0, 4, 4}},
		{RSQUARE, "]", position{"punctuation", 0, 5, 5}},
		{COMMA, ",", position{"punctuation", 0, 6, 6}},
		{EOF, "", position{"punctuation", 0, 7, 7}}}},
	{"operators", "- + = ++ += < > <= >= == != * / << >> & | ^ ~ ** % || && =~ !~ --", []token{
		{MINUS, "-", position{"operators", 0, 0, 0}},
		{PLUS, "+", position{"operators", 0, 2, 2}},
		{ASSIGN, "=", position{"operators", 0, 4, 4}},
		{INC, "++", position{"operators", 0, 6, 7}},
		{ADD_ASSIGN, "+=", position{"operators", 0, 9, 10}},
		{LT, "<", position{"operators", 0, 12, 12}},
		{GT, ">", position{"operators", 0, 14, 14}},
		{LE, "<=", position{"operators", 0, 16, 17}},
		{GE, ">=", position{"operators", 0, 19, 20}},
		{EQ, "==", position{"operators", 0, 22, 23}},
		{NE, "!=", position{"operators", 0, 25, 26}},
		{MUL, "*", position{"operators", 0, 28, 28}},
		{DIV, "/", position{"operators", 0, 30, 30}},
		{SHL, "<<", position{"operators", 0, 32, 33}},
		{SHR, ">>", position{"operators", 0, 35, 36}},
		{BITAND, "&", position{"operators", 0, 38, 38}},
		{BITOR, "|", position{"operators", 0, 40, 40}},
		{XOR, "^", position{"operators", 0, 42, 42}},
		{NOT, "~", position{"operators", 0, 44, 44}},
		{POW, "**", position{"operators", 0, 46, 47}},
		{MOD, "%", position{"operators", 0, 49, 49}},
		{OR, "||", position{"operators", 0, 51, 52}},
		{AND, "&&", position{"operators", 0, 54, 55}},
		{MATCH, "=~", position{"operators", 0, 57, 58}},
		{NOT_MATCH, "!~", position{"operators", 0, 60, 61}},
		{DEC, "--", position{"operators", 0, 63, 64}},
		{EOF, "", position{"operators", 0, 65, 65}}}},
	{"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\ntimer\notherwise\nelse\ndel\ntext\nafter\n", []token{
			{COUNTER, "counter", position{"keywords", 0, 0, 6}},
			{NL, "\n", position{"keywords", 1, 7, -1}},
			{GAUGE, "gauge", position{"keywords", 1, 0, 4}},
			{NL, "\n", position{"keywords", 2, 5, -1}},
			{AS, "as", position{"keywords", 2, 0, 1}},
			{NL, "\n", position{"keywords", 3, 2, -1}},
			{BY, "by", position{"keywords", 3, 0, 1}},
			{NL, "\n", position{"keywords", 4, 2, -1}},
			{HIDDEN, "hidden", position{"keywords", 4, 0, 5}},
			{NL, "\n", position{"keywords", 5, 6, -1}},
			{DEF, "def", position{"keywords", 5, 0, 2}},
			{NL, "\n", position{"keywords", 6, 3, -1}},
			{NEXT, "next", position{"keywords", 6, 0, 3}},
			{NL, "\n", position{"keywords", 7, 4, -1}},
			{CONST, "const", position{"keywords", 7, 0, 4}},
			{NL, "\n", position{"keywords", 8, 5, -1}},
			{TIMER, "timer", position{"keywords", 8, 0, 4}},
			{NL, "\n", position{"keywords", 9, 5, -1}},
			{OTHERWISE, "otherwise", position{"keywords", 9, 0, 8}},
			{NL, "\n", position{"keywords", 10, 9, -1}},
			{ELSE, "else", position{"keywords", 10, 0, 3}},
			{NL, "\n", position{"keywords", 11, 4, -1}},
			{DEL, "del", position{"keywords", 11, 0, 2}},
			{NL, "\n", position{"keywords", 12, 3, -1}},
			{TEXT, "text", position{"keywords", 12, 0, 3}},
			{NL, "\n", position{"keywords", 13, 4, -1}},
			{AFTER, "after", position{"keywords", 13, 0, 4}},
			{NL, "\n", position{"keywords", 14, 5, -1}},
			{EOF, "", position{"keywords", 14, 0, 0}}}},
	{"builtins",
		"strptime\ntimestamp\ntolower\nlen\nstrtol\nsettime\ngetfilename\nint\nbool\nfloat\nstring\n", []token{
			{BUILTIN, "strptime", position{"builtins", 0, 0, 7}},
			{NL, "\n", position{"builtins", 1, 8, -1}},
			{BUILTIN, "timestamp", position{"builtins", 1, 0, 8}},
			{NL, "\n", position{"builtins", 2, 9, -1}},
			{BUILTIN, "tolower", position{"builtins", 2, 0, 6}},
			{NL, "\n", position{"builtins", 3, 7, -1}},
			{BUILTIN, "len", position{"builtins", 3, 0, 2}},
			{NL, "\n", position{"builtins", 4, 3, -1}},
			{BUILTIN, "strtol", position{"builtins", 4, 0, 5}},
			{NL, "\n", position{"builtins", 5, 6, -1}},
			{BUILTIN, "settime", position{"builtins", 5, 0, 6}},
			{NL, "\n", position{"builtins", 6, 7, -1}},
			{BUILTIN, "getfilename", position{"builtins", 6, 0, 10}},
			{NL, "\n", position{"builtins", 7, 11, -1}},
			{BUILTIN, "int", position{"builtins", 7, 0, 2}},
			{NL, "\n", position{"builtins", 8, 3, -1}},
			{BUILTIN, "bool", position{"builtins", 8, 0, 3}},
			{NL, "\n", position{"builtins", 9, 4, -1}},
			{BUILTIN, "float", position{"builtins", 9, 0, 4}},
			{NL, "\n", position{"builtins", 10, 5, -1}},
			{BUILTIN, "string", position{"builtins", 10, 0, 5}},
			{NL, "\n", position{"builtins", 11, 6, -1}},
			{EOF, "", position{"builtins", 11, 0, 0}}}},
	{"numbers", "1 23 3.14 1.61.1 -1 -1.0 1h 0d 3d -1.5h 15m 24h0m0s", []token{
		{INTLITERAL, "1", position{"numbers", 0, 0, 0}},
		{INTLITERAL, "23", position{"numbers", 0, 2, 3}},
		{FLOATLITERAL, "3.14", position{"numbers", 0, 5, 8}},
		{FLOATLITERAL, "1.61", position{"numbers", 0, 10, 13}},
		{FLOATLITERAL, ".1", position{"numbers", 0, 14, 15}},
		{INTLITERAL, "-1", position{"numbers", 0, 17, 18}},
		{FLOATLITERAL, "-1.0", position{"numbers", 0, 20, 23}},
		{DURATIONLITERAL, "1h", position{"numbers", 0, 25, 26}},
		{DURATIONLITERAL, "0d", position{"numbers", 0, 28, 29}},
		{DURATIONLITERAL, "3d", position{"numbers", 0, 31, 32}},
		{DURATIONLITERAL, "-1.5h", position{"numbers", 0, 34, 38}},
		{DURATIONLITERAL, "15m", position{"numbers", 0, 40, 42}},
		{DURATIONLITERAL, "24h0m0s", position{"numbers", 0, 44, 50}},
		{EOF, "", position{"numbers", 0, 51, 51}},
	}},
	{"identifier", "a be foo\nquux line_count", []token{
		{ID, "a", position{"identifier", 0, 0, 0}},
		{ID, "be", position{"identifier", 0, 2, 3}},
		{ID, "foo", position{"identifier", 0, 5, 7}},
		{NL, "\n", position{"identifier", 1, 8, -1}},
		{ID, "quux", position{"identifier", 1, 0, 3}},
		{ID, "line_count", position{"identifier", 1, 5, 14}},
		{EOF, "", position{"identifier", 1, 15, 15}}}},
	{"regex", "/asdf/", []token{
		{DIV, "/", position{"regex", 0, 0, 0}},
		{REGEX, "asdf", position{"regex", 0, 1, 4}},
		{DIV, "/", position{"regex", 0, 5, 5}},
		{EOF, "", position{"regex", 0, 6, 6}}}},
	{"regex with escape", `/asdf\//`, []token{
		{DIV, "/", position{"regex with escape", 0, 0, 0}},
		{REGEX, `asdf/`, position{"regex with escape", 0, 1, 6}},
		{DIV, "/", position{"regex with escape", 0, 7, 7}},
		{EOF, "", position{"regex with escape", 0, 8, 8}}}},
	{"regex with escape and special char", `/foo\d\//`, []token{
		{DIV, "/", position{"regex with escape and special char", 0, 0, 0}},
		{REGEX, `foo\d/`, position{"regex with escape and special char", 0, 1, 7}},
		{DIV, "/", position{"regex with escape and special char", 0, 8, 8}},
		{EOF, "", position{"regex with escape and special char", 0, 9, 9}}}},
	{"capref", "$foo $1", []token{
		{CAPREF_NAMED, "foo", position{"capref", 0, 0, 3}},
		{CAPREF, "1", position{"capref", 0, 5, 6}},
		{EOF, "", position{"capref", 0, 7, 7}}}},
	{"numerical capref", "$1", []token{
		{CAPREF, "1", position{"numerical capref", 0, 0, 1}},
		{EOF, "", position{"numerical capref", 0, 2, 2}}}},
	{"capref with trailing punc", "$foo,", []token{
		{CAPREF_NAMED, "foo", position{"capref with trailing punc", 0, 0, 3}},
		{COMMA, ",", position{"capref with trailing punc", 0, 4, 4}},
		{EOF, "", position{"capref with trailing punc", 0, 5, 5}}}},
	{"quoted string", `"asdf"`, []token{
		{STRING, `asdf`, position{"quoted string", 0, 0, 5}},
		{EOF, "", position{"quoted string", 0, 6, 6}}}},
	{"escaped quote in quoted string", `"\""`, []token{
		{STRING, `"`, position{"escaped quote in quoted string", 0, 0, 3}},
		{EOF, "", position{"escaped quote in quoted string", 0, 4, 4}}}},
	{"decorator", `@foo`, []token{
		{DECO, "foo", position{"decorator", 0, 0, 3}},
		{EOF, "", position{"decorator", 0, 4, 4}}}},
	{"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}", []token{
			{DIV, "/", position{"large program", 0, 0, 0}},
			{REGEX, "(?P<date>[[:digit:]-/ ])", position{"large program", 0, 1, 25}},
			{DIV, "/", position{"large program", 0, 26, 26}},
			{LCURLY, "{", position{"large program", 0, 28, 28}},
			{NL, "\n", position{"large program", 1, 29, -1}},
			{BUILTIN, "strptime", position{"large program", 1, 2, 9}},
			{LPAREN, "(", position{"large program", 1, 10, 10}},
			{CAPREF_NAMED, "date", position{"large program", 1, 11, 15}},
			{COMMA, ",", position{"large program", 1, 16, 16}},
			{STRING, "%Y/%m/%d %H:%M:%S", position{"large program", 1, 18, 36}},
			{RPAREN, ")", position{"large program", 1, 37, 37}},
			{NL, "\n", position{"large program", 2, 38, -1}},
			{ID, "foo", position{"large program", 2, 2, 4}},
			{INC, "++", position{"large program", 2, 5, 6}},
			{NL, "\n", position{"large program", 3, 7, -1}},
			{RCURLY, "}", position{"large program", 3, 0, 0}},
			{EOF, "", position{"large program", 3, 1, 1}}}},
	{"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo", []token{
			{NL, "\n", position{"linecount", 3, 12, -1}},
			{ID, "foo", position{"linecount", 3, 0, 2}},
			{EOF, "", position{"linecount", 3, 3, 3}}}},
	// errors
	{"unexpected char", "?", []token{
		{INVALID, "Unexpected input: '?'", position{"unexpected char", 0, 0, 0}},
		{EOF, "", position{"unexpected char", 0, 1, 1}}}},
	{"unterminated regex", "/foo\n", []token{
		{DIV, "/", position{"unterminated regex", 0, 0, 0}},
		{INVALID, "Unterminated regular expression: \"/foo\"", position{"unterminated regex", 0, 1, 3}},
		{EOF, "", position{"unterminated regex", 0, 4, 4}}}},
	{"unterminated quoted string", "\"foo\n", []token{
		{INVALID, "Unterminated quoted string: \"\\\"foo\"", position{"unterminated quoted string", 0, 0, 3}},
		{EOF, "", position{"unterminated quoted string", 0, 4, 4}}}},
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

			if diff := go_cmp.Diff(tc.tokens, tokens, go_cmp.AllowUnexported(token{}, position{})); diff != "" {
				t.Errorf("-expected +received\n%s", diff)
				t.Logf("received: %v", tokens)
			}
		})
	}
}
