// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"reflect"
	"strings"
	"testing"
)

type lexerTest struct {
	name   string
	input  string
	tokens []token
}

var lexerTests = []lexerTest{
	{"empty", "", []token{
		token{EOF, "", position{0, 0, 0}}}},
	{"spaces", " \t\n", []token{
		token{EOF, "", position{1, 0, 0}}}},
	{"comment", "# comment", []token{
		token{EOF, "", position{0, 9, 9}}}},
	{"comment not at col 1", "  # comment", []token{
		token{EOF, "", position{0, 11, 11}}}},
	{"punctuation", "{}()[],", []token{
		token{LCURLY, "{", position{0, 0, 0}},
		token{RCURLY, "}", position{0, 1, 1}},
		token{LPAREN, "(", position{0, 2, 2}},
		token{RPAREN, ")", position{0, 3, 3}},
		token{LSQUARE, "[", position{0, 4, 4}},
		token{RSQUARE, "]", position{0, 5, 5}},
		token{COMMA, ",", position{0, 6, 6}},
		token{EOF, "", position{0, 7, 7}}}},
	{"operators", "- + = ++ += < > <= >= == !=", []token{
		token{MINUS, "-", position{0, 0, 0}},
		token{PLUS, "+", position{0, 2, 2}},
		token{ASSIGN, "=", position{0, 4, 4}},
		token{INC, "++", position{0, 6, 7}},
		token{ADD_ASSIGN, "+=", position{0, 9, 10}},
		token{LT, "<", position{0, 12, 12}},
		token{GT, ">", position{0, 14, 14}},
		token{LE, "<=", position{0, 16, 17}},
		token{GE, ">=", position{0, 19, 20}},
		token{EQ, "==", position{0, 22, 23}},
		token{NE, "!=", position{0, 25, 26}},
		token{EOF, "", position{0, 27, 27}}}},
	{"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\n", []token{
			token{COUNTER, "counter", position{0, 0, 6}},
			token{GAUGE, "gauge", position{1, 0, 4}},
			token{AS, "as", position{2, 0, 1}},
			token{BY, "by", position{3, 0, 1}},
			token{HIDDEN, "hidden", position{4, 0, 5}},
			token{DEF, "def", position{5, 0, 2}},
			token{NEXT, "next", position{6, 0, 3}},
			token{CONST, "const", position{7, 0, 4}},
			token{EOF, "", position{8, 0, 0}}}},
	{"builtins",
		"strptime\ntimestamp\ntolower\nlen\n", []token{
			token{BUILTIN, "strptime", position{0, 0, 7}},
			token{BUILTIN, "timestamp", position{1, 0, 8}},
			token{BUILTIN, "tolower", position{2, 0, 6}},
			token{BUILTIN, "len", position{3, 0, 2}},
			token{EOF, "", position{4, 0, 0}}}},
	{"numeric", "1 23", []token{
		token{NUMERIC, "1", position{0, 0, 0}},
		token{NUMERIC, "23", position{0, 2, 3}},
		token{EOF, "", position{0, 4, 4}}}},
	{"identifer", "a be foo\nquux line-count", []token{
		token{ID, "a", position{0, 0, 0}},
		token{ID, "be", position{0, 2, 3}},
		token{ID, "foo", position{0, 5, 7}},
		token{ID, "quux", position{1, 0, 3}},
		token{ID, "line-count", position{1, 5, 14}},
		token{EOF, "", position{1, 15, 15}}}},
	{"regex", "/asdf/", []token{
		token{REGEX, "asdf", position{0, 0, 5}},
		token{EOF, "", position{0, 6, 6}}}},
	{"regex with escape", `/asdf\//`, []token{
		token{REGEX, `asdf/`, position{0, 0, 7}},
		token{EOF, "", position{0, 8, 8}}}},
	{"regex with escape and special char", `/foo\d\//`, []token{
		token{REGEX, `foo\d/`, position{0, 0, 8}},
		token{EOF, "", position{0, 9, 9}}}},
	{"capref", "$foo", []token{
		token{CAPREF, "foo", position{0, 0, 3}},
		token{EOF, "", position{0, 4, 4}}}},
	{"numerical capref", "$1", []token{
		token{CAPREF, "1", position{0, 0, 1}},
		token{EOF, "", position{0, 2, 2}}}},
	{"capref with trailing punc", "$foo,", []token{
		token{CAPREF, "foo", position{0, 0, 3}},
		token{COMMA, ",", position{0, 4, 4}},
		token{EOF, "", position{0, 5, 5}}}},
	{"quoted string", `"asdf"`, []token{
		token{STRING, `asdf`, position{0, 0, 5}},
		token{EOF, "", position{0, 6, 6}}}},
	{"escaped quote in quoted string", `"\""`, []token{
		token{STRING, `"`, position{0, 0, 3}},
		token{EOF, "", position{0, 4, 4}}}},
	{"decorator", `@foo`, []token{
		token{DECO, "foo", position{0, 0, 3}},
		token{EOF, "", position{0, 4, 4}}}},
	{"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}", []token{
			token{REGEX, "(?P<date>[[:digit:]-/ ])", position{0, 0, 26}},
			token{LCURLY, "{", position{0, 28, 28}},
			token{BUILTIN, "strptime", position{1, 2, 9}},
			token{LPAREN, "(", position{1, 10, 10}},
			token{CAPREF, "date", position{1, 11, 15}},
			token{COMMA, ",", position{1, 16, 16}},
			token{STRING, "%Y/%m/%d %H:%M:%S", position{1, 18, 36}},
			token{RPAREN, ")", position{1, 37, 37}},
			token{ID, "foo", position{2, 2, 4}},
			token{INC, "++", position{2, 5, 6}},
			token{RCURLY, "}", position{3, 0, 0}},
			token{EOF, "", position{3, 1, 1}}}},
	{"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo", []token{
			token{ID, "foo", position{3, 0, 2}},
			token{EOF, "", position{3, 3, 3}}}},
	// errors
	{"unexpected char", "?", []token{
		token{INVALID, "Unexpected input: '?'", position{0, 0, 0}}}},
	{"unterminated regex", "/foo\n", []token{
		token{INVALID, "Unterminated regular expression: \"/foo\"", position{0, 0, 3}}}},
	{"unterminated quoted string", "\"foo\n", []token{
		token{INVALID, "Unterminated quoted string: \"\\\"foo\"", position{0, 0, 3}}}},
}

// collect gathers the emitted items into a slice.
func collect(t *lexerTest) (tokens []token) {
	l := newLexer(t.name, strings.NewReader(t.input))
	for {
		token := l.nextToken()
		tokens = append(tokens, token)
		if token.kind == EOF || token.kind == INVALID {
			break
		}
	}
	return
}

func TestLex(t *testing.T) {
	for _, test := range lexerTests {
		tokens := collect(&test)
		if !reflect.DeepEqual(tokens, test.tokens) {
			t.Errorf("%s: got\n\t%v\nexpected\n\t%v", test.name, tokens, test.tokens)
		}
	}
}
