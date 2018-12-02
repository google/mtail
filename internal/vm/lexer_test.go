// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm_test

import (
	"strings"
	"testing"

	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm"
	"github.com/google/mtail/internal/vm/position"
)

type lexerTest struct {
	name   string
	input  string
	tokens []vm.Token
}

var lexerTests = []lexerTest{
	{"empty", "", []vm.Token{
		{vm.EOF, "", position.Position{"empty", 0, 0, 0}}}},
	{"spaces", " \t", []vm.Token{
		{vm.EOF, "", position.Position{"spaces", 0, 2, 2}}}},
	{"newlines", "\n", []vm.Token{
		{vm.NL, "\n", position.Position{"newlines", 1, 0, -1}},
		{vm.EOF, "", position.Position{"newlines", 1, 0, 0}}}},
	{"comment", "# comment", []vm.Token{
		{vm.EOF, "", position.Position{"comment", 0, 9, 9}}}},
	{"comment not at col 1", "  # comment", []vm.Token{
		{vm.EOF, "", position.Position{"comment not at col 1", 0, 11, 11}}}},
	{"punctuation", "{}()[],", []vm.Token{
		{vm.LCURLY, "{", position.Position{"punctuation", 0, 0, 0}},
		{vm.RCURLY, "}", position.Position{"punctuation", 0, 1, 1}},
		{vm.LPAREN, "(", position.Position{"punctuation", 0, 2, 2}},
		{vm.RPAREN, ")", position.Position{"punctuation", 0, 3, 3}},
		{vm.LSQUARE, "[", position.Position{"punctuation", 0, 4, 4}},
		{vm.RSQUARE, "]", position.Position{"punctuation", 0, 5, 5}},
		{vm.COMMA, ",", position.Position{"punctuation", 0, 6, 6}},
		{vm.EOF, "", position.Position{"punctuation", 0, 7, 7}}}},
	{"operators", "- + = ++ += < > <= >= == != * / << >> & | ^ ~ ** % || && =~ !~ --", []vm.Token{
		{vm.MINUS, "-", position.Position{"operators", 0, 0, 0}},
		{vm.PLUS, "+", position.Position{"operators", 0, 2, 2}},
		{vm.ASSIGN, "=", position.Position{"operators", 0, 4, 4}},
		{vm.INC, "++", position.Position{"operators", 0, 6, 7}},
		{vm.ADD_ASSIGN, "+=", position.Position{"operators", 0, 9, 10}},
		{vm.LT, "<", position.Position{"operators", 0, 12, 12}},
		{vm.GT, ">", position.Position{"operators", 0, 14, 14}},
		{vm.LE, "<=", position.Position{"operators", 0, 16, 17}},
		{vm.GE, ">=", position.Position{"operators", 0, 19, 20}},
		{vm.EQ, "==", position.Position{"operators", 0, 22, 23}},
		{vm.NE, "!=", position.Position{"operators", 0, 25, 26}},
		{vm.MUL, "*", position.Position{"operators", 0, 28, 28}},
		{vm.DIV, "/", position.Position{"operators", 0, 30, 30}},
		{vm.SHL, "<<", position.Position{"operators", 0, 32, 33}},
		{vm.SHR, ">>", position.Position{"operators", 0, 35, 36}},
		{vm.BITAND, "&", position.Position{"operators", 0, 38, 38}},
		{vm.BITOR, "|", position.Position{"operators", 0, 40, 40}},
		{vm.XOR, "^", position.Position{"operators", 0, 42, 42}},
		{vm.NOT, "~", position.Position{"operators", 0, 44, 44}},
		{vm.POW, "**", position.Position{"operators", 0, 46, 47}},
		{vm.MOD, "%", position.Position{"operators", 0, 49, 49}},
		{vm.OR, "||", position.Position{"operators", 0, 51, 52}},
		{vm.AND, "&&", position.Position{"operators", 0, 54, 55}},
		{vm.MATCH, "=~", position.Position{"operators", 0, 57, 58}},
		{vm.NOT_MATCH, "!~", position.Position{"operators", 0, 60, 61}},
		{vm.DEC, "--", position.Position{"operators", 0, 63, 64}},
		{vm.EOF, "", position.Position{"operators", 0, 65, 65}}}},
	{"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\ntimer\notherwise\nelse\ndel\ntext\nafter\nstop\n", []vm.Token{
			{vm.COUNTER, "counter", position.Position{"keywords", 0, 0, 6}},
			{vm.NL, "\n", position.Position{"keywords", 1, 7, -1}},
			{vm.GAUGE, "gauge", position.Position{"keywords", 1, 0, 4}},
			{vm.NL, "\n", position.Position{"keywords", 2, 5, -1}},
			{vm.AS, "as", position.Position{"keywords", 2, 0, 1}},
			{vm.NL, "\n", position.Position{"keywords", 3, 2, -1}},
			{vm.BY, "by", position.Position{"keywords", 3, 0, 1}},
			{vm.NL, "\n", position.Position{"keywords", 4, 2, -1}},
			{vm.HIDDEN, "hidden", position.Position{"keywords", 4, 0, 5}},
			{vm.NL, "\n", position.Position{"keywords", 5, 6, -1}},
			{vm.DEF, "def", position.Position{"keywords", 5, 0, 2}},
			{vm.NL, "\n", position.Position{"keywords", 6, 3, -1}},
			{vm.NEXT, "next", position.Position{"keywords", 6, 0, 3}},
			{vm.NL, "\n", position.Position{"keywords", 7, 4, -1}},
			{vm.CONST, "const", position.Position{"keywords", 7, 0, 4}},
			{vm.NL, "\n", position.Position{"keywords", 8, 5, -1}},
			{vm.TIMER, "timer", position.Position{"keywords", 8, 0, 4}},
			{vm.NL, "\n", position.Position{"keywords", 9, 5, -1}},
			{vm.OTHERWISE, "otherwise", position.Position{"keywords", 9, 0, 8}},
			{vm.NL, "\n", position.Position{"keywords", 10, 9, -1}},
			{vm.ELSE, "else", position.Position{"keywords", 10, 0, 3}},
			{vm.NL, "\n", position.Position{"keywords", 11, 4, -1}},
			{vm.DEL, "del", position.Position{"keywords", 11, 0, 2}},
			{vm.NL, "\n", position.Position{"keywords", 12, 3, -1}},
			{vm.TEXT, "text", position.Position{"keywords", 12, 0, 3}},
			{vm.NL, "\n", position.Position{"keywords", 13, 4, -1}},
			{vm.AFTER, "after", position.Position{"keywords", 13, 0, 4}},
			{vm.NL, "\n", position.Position{"keywords", 14, 5, -1}},
			{vm.STOP, "stop", position.Position{"keywords", 14, 0, 3}},
			{vm.NL, "\n", position.Position{"keywords", 15, 4, -1}},
			{vm.EOF, "", position.Position{"keywords", 15, 0, 0}}}},
	{"builtins",
		"strptime\ntimestamp\ntolower\nlen\nstrtol\nsettime\ngetfilename\nint\nbool\nfloat\nstring\n", []vm.Token{
			{vm.BUILTIN, "strptime", position.Position{"builtins", 0, 0, 7}},
			{vm.NL, "\n", position.Position{"builtins", 1, 8, -1}},
			{vm.BUILTIN, "timestamp", position.Position{"builtins", 1, 0, 8}},
			{vm.NL, "\n", position.Position{"builtins", 2, 9, -1}},
			{vm.BUILTIN, "tolower", position.Position{"builtins", 2, 0, 6}},
			{vm.NL, "\n", position.Position{"builtins", 3, 7, -1}},
			{vm.BUILTIN, "len", position.Position{"builtins", 3, 0, 2}},
			{vm.NL, "\n", position.Position{"builtins", 4, 3, -1}},
			{vm.BUILTIN, "strtol", position.Position{"builtins", 4, 0, 5}},
			{vm.NL, "\n", position.Position{"builtins", 5, 6, -1}},
			{vm.BUILTIN, "settime", position.Position{"builtins", 5, 0, 6}},
			{vm.NL, "\n", position.Position{"builtins", 6, 7, -1}},
			{vm.BUILTIN, "getfilename", position.Position{"builtins", 6, 0, 10}},
			{vm.NL, "\n", position.Position{"builtins", 7, 11, -1}},
			{vm.BUILTIN, "int", position.Position{"builtins", 7, 0, 2}},
			{vm.NL, "\n", position.Position{"builtins", 8, 3, -1}},
			{vm.BUILTIN, "bool", position.Position{"builtins", 8, 0, 3}},
			{vm.NL, "\n", position.Position{"builtins", 9, 4, -1}},
			{vm.BUILTIN, "float", position.Position{"builtins", 9, 0, 4}},
			{vm.NL, "\n", position.Position{"builtins", 10, 5, -1}},
			{vm.BUILTIN, "string", position.Position{"builtins", 10, 0, 5}},
			{vm.NL, "\n", position.Position{"builtins", 11, 6, -1}},
			{vm.EOF, "", position.Position{"builtins", 11, 0, 0}}}},
	{"numbers", "1 23 3.14 1.61.1 -1 -1.0 1h 0d 3d -1.5h 15m 24h0m0s", []vm.Token{
		{vm.INTLITERAL, "1", position.Position{"numbers", 0, 0, 0}},
		{vm.INTLITERAL, "23", position.Position{"numbers", 0, 2, 3}},
		{vm.FLOATLITERAL, "3.14", position.Position{"numbers", 0, 5, 8}},
		{vm.FLOATLITERAL, "1.61", position.Position{"numbers", 0, 10, 13}},
		{vm.FLOATLITERAL, ".1", position.Position{"numbers", 0, 14, 15}},
		{vm.INTLITERAL, "-1", position.Position{"numbers", 0, 17, 18}},
		{vm.FLOATLITERAL, "-1.0", position.Position{"numbers", 0, 20, 23}},
		{vm.DURATIONLITERAL, "1h", position.Position{"numbers", 0, 25, 26}},
		{vm.DURATIONLITERAL, "0d", position.Position{"numbers", 0, 28, 29}},
		{vm.DURATIONLITERAL, "3d", position.Position{"numbers", 0, 31, 32}},
		{vm.DURATIONLITERAL, "-1.5h", position.Position{"numbers", 0, 34, 38}},
		{vm.DURATIONLITERAL, "15m", position.Position{"numbers", 0, 40, 42}},
		{vm.DURATIONLITERAL, "24h0m0s", position.Position{"numbers", 0, 44, 50}},
		{vm.EOF, "", position.Position{"numbers", 0, 51, 51}},
	}},
	{"identifier", "a be foo\nquux line_count", []vm.Token{
		{vm.ID, "a", position.Position{"identifier", 0, 0, 0}},
		{vm.ID, "be", position.Position{"identifier", 0, 2, 3}},
		{vm.ID, "foo", position.Position{"identifier", 0, 5, 7}},
		{vm.NL, "\n", position.Position{"identifier", 1, 8, -1}},
		{vm.ID, "quux", position.Position{"identifier", 1, 0, 3}},
		{vm.ID, "line_count", position.Position{"identifier", 1, 5, 14}},
		{vm.EOF, "", position.Position{"identifier", 1, 15, 15}}}},
	{"regex", "/asdf/", []vm.Token{
		{vm.DIV, "/", position.Position{"regex", 0, 0, 0}},
		{vm.REGEX, "asdf", position.Position{"regex", 0, 1, 4}},
		{vm.DIV, "/", position.Position{"regex", 0, 5, 5}},
		{vm.EOF, "", position.Position{"regex", 0, 6, 6}}}},
	{"regex with escape", `/asdf\//`, []vm.Token{
		{vm.DIV, "/", position.Position{"regex with escape", 0, 0, 0}},
		{vm.REGEX, `asdf/`, position.Position{"regex with escape", 0, 1, 6}},
		{vm.DIV, "/", position.Position{"regex with escape", 0, 7, 7}},
		{vm.EOF, "", position.Position{"regex with escape", 0, 8, 8}}}},
	{"regex with escape and special char", `/foo\d\//`, []vm.Token{
		{vm.DIV, "/", position.Position{"regex with escape and special char", 0, 0, 0}},
		{vm.REGEX, `foo\d/`, position.Position{"regex with escape and special char", 0, 1, 7}},
		{vm.DIV, "/", position.Position{"regex with escape and special char", 0, 8, 8}},
		{vm.EOF, "", position.Position{"regex with escape and special char", 0, 9, 9}}}},
	{"capref", "$foo $1", []vm.Token{
		{vm.CAPREF_NAMED, "foo", position.Position{"capref", 0, 0, 3}},
		{vm.CAPREF, "1", position.Position{"capref", 0, 5, 6}},
		{vm.EOF, "", position.Position{"capref", 0, 7, 7}}}},
	{"numerical capref", "$1", []vm.Token{
		{vm.CAPREF, "1", position.Position{"numerical capref", 0, 0, 1}},
		{vm.EOF, "", position.Position{"numerical capref", 0, 2, 2}}}},
	{"capref with trailing punc", "$foo,", []vm.Token{
		{vm.CAPREF_NAMED, "foo", position.Position{"capref with trailing punc", 0, 0, 3}},
		{vm.COMMA, ",", position.Position{"capref with trailing punc", 0, 4, 4}},
		{vm.EOF, "", position.Position{"capref with trailing punc", 0, 5, 5}}}},
	{"quoted string", `"asdf"`, []vm.Token{
		{vm.STRING, `asdf`, position.Position{"quoted string", 0, 0, 5}},
		{vm.EOF, "", position.Position{"quoted string", 0, 6, 6}}}},
	{"escaped quote in quoted string", `"\""`, []vm.Token{
		{vm.STRING, `"`, position.Position{"escaped quote in quoted string", 0, 0, 3}},
		{vm.EOF, "", position.Position{"escaped quote in quoted string", 0, 4, 4}}}},
	{"decorator", `@foo`, []vm.Token{
		{vm.DECO, "foo", position.Position{"decorator", 0, 0, 3}},
		{vm.EOF, "", position.Position{"decorator", 0, 4, 4}}}},
	{"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}", []vm.Token{
			{vm.DIV, "/", position.Position{"large program", 0, 0, 0}},
			{vm.REGEX, "(?P<date>[[:digit:]-/ ])", position.Position{"large program", 0, 1, 25}},
			{vm.DIV, "/", position.Position{"large program", 0, 26, 26}},
			{vm.LCURLY, "{", position.Position{"large program", 0, 28, 28}},
			{vm.NL, "\n", position.Position{"large program", 1, 29, -1}},
			{vm.BUILTIN, "strptime", position.Position{"large program", 1, 2, 9}},
			{vm.LPAREN, "(", position.Position{"large program", 1, 10, 10}},
			{vm.CAPREF_NAMED, "date", position.Position{"large program", 1, 11, 15}},
			{vm.COMMA, ",", position.Position{"large program", 1, 16, 16}},
			{vm.STRING, "%Y/%m/%d %H:%M:%S", position.Position{"large program", 1, 18, 36}},
			{vm.RPAREN, ")", position.Position{"large program", 1, 37, 37}},
			{vm.NL, "\n", position.Position{"large program", 2, 38, -1}},
			{vm.ID, "foo", position.Position{"large program", 2, 2, 4}},
			{vm.INC, "++", position.Position{"large program", 2, 5, 6}},
			{vm.NL, "\n", position.Position{"large program", 3, 7, -1}},
			{vm.RCURLY, "}", position.Position{"large program", 3, 0, 0}},
			{vm.EOF, "", position.Position{"large program", 3, 1, 1}}}},
	{"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo", []vm.Token{
			{vm.NL, "\n", position.Position{"linecount", 3, 12, -1}},
			{vm.ID, "foo", position.Position{"linecount", 3, 0, 2}},
			{vm.EOF, "", position.Position{"linecount", 3, 3, 3}}}},
	// errors
	{"unexpected char", "?", []vm.Token{
		{vm.INVALID, "Unexpected input: '?'", position.Position{"unexpected char", 0, 0, 0}},
		{vm.EOF, "", position.Position{"unexpected char", 0, 1, 1}}}},
	{"unterminated regex", "/foo\n", []vm.Token{
		{vm.DIV, "/", position.Position{"unterminated regex", 0, 0, 0}},
		{vm.INVALID, "Unterminated regular expression: \"/foo\"", position.Position{"unterminated regex", 0, 1, 3}},
		{vm.EOF, "", position.Position{"unterminated regex", 0, 4, 4}}}},
	{"unterminated quoted string", "\"foo\n", []vm.Token{
		{vm.INVALID, "Unterminated quoted string: \"\\\"foo\"", position.Position{"unterminated quoted string", 0, 0, 3}},
		{vm.EOF, "", position.Position{"unterminated quoted string", 0, 4, 4}}}},
}

// collect gathers the emitted items into a slice.
func collect(t *lexerTest) (tokens []vm.Token) {
	// Hack to count divs seen for regex tests.
	inRegexSet := false
	l := vm.NewLexer(t.name, strings.NewReader(t.input))
	for {
		tok := l.NextToken()
		// Hack to simulate context signal from parser.
		if tok.Kind == vm.DIV && (strings.Contains(t.name, "regex") || strings.HasPrefix(t.name, "large program")) && !inRegexSet {
			l.InRegex = true
			inRegexSet = true
		}
		tokens = append(tokens, tok)
		if tok.Kind == vm.EOF {
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

			if diff := testutil.Diff(tc.tokens, tokens, testutil.AllowUnexported(vm.Token{}, position.Position{})); diff != "" {
				t.Errorf("-expected +received\n%s", diff)
				t.Logf("received: %v", tokens)
			}
		})
	}
}
