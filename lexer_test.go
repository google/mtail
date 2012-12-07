// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"reflect"
	"strings"
	"testing"
)

type lexerTest struct {
	name   string
	input  string
	tokens []Token
}

var lexerTests = []lexerTest{
	{"empty", "", []Token{
		Token{EOF, "", Position{0, 0, 0}}}},
	{"spaces", " \t\n", []Token{
		Token{EOF, "", Position{1, 0, 0}}}},
	{"comment", "# comment", []Token{
		Token{EOF, "", Position{0, 9, 9}}}},
	{"comment not at col 1", "  # comment", []Token{
		Token{EOF, "", Position{0, 11, 11}}}},
	{"punctuation", "{}()[],", []Token{
		Token{LCURLY, "{", Position{0, 0, 0}},
		Token{RCURLY, "}", Position{0, 1, 1}},
		Token{LPAREN, "(", Position{0, 2, 2}},
		Token{RPAREN, ")", Position{0, 3, 3}},
		Token{LSQUARE, "[", Position{0, 4, 4}},
		Token{RSQUARE, "]", Position{0, 5, 5}},
		Token{COMMA, ",", Position{0, 6, 6}},
		Token{EOF, "", Position{0, 7, 7}}}},
	{"operators", "- + = ++ += < > <= >= == !=", []Token{
		Token{MINUS, "-", Position{0, 0, 0}},
		Token{PLUS, "+", Position{0, 2, 2}},
		Token{ASSIGN, "=", Position{0, 4, 4}},
		Token{INC, "++", Position{0, 6, 7}},
		Token{ADD_ASSIGN, "+=", Position{0, 9, 10}},
		Token{LT, "<", Position{0, 12, 12}},
		Token{GT, ">", Position{0, 14, 14}},
		Token{LE, "<=", Position{0, 16, 17}},
		Token{GE, ">=", Position{0, 19, 20}},
		Token{EQ, "==", Position{0, 22, 23}},
		Token{NE, "!=", Position{0, 25, 26}},
		Token{EOF, "", Position{0, 27, 27}}}},
	{"keywords",
		"counter\ngauge\nas\nby\nhidden\ndef\nnext\nconst\n", []Token{
			Token{COUNTER, "counter", Position{0, 0, 6}},
			Token{GAUGE, "gauge", Position{1, 0, 4}},
			Token{AS, "as", Position{2, 0, 1}},
			Token{BY, "by", Position{3, 0, 1}},
			Token{HIDDEN, "hidden", Position{4, 0, 5}},
			Token{DEF, "def", Position{5, 0, 2}},
			Token{NEXT, "next", Position{6, 0, 3}},
			Token{CONST, "const", Position{7, 0, 4}},
			Token{EOF, "", Position{8, 0, 0}}}},
	{"builtins",
		"strptime\ntimestamp\ntolower\nlen\n", []Token{
			Token{BUILTIN, "strptime", Position{0, 0, 7}},
			Token{BUILTIN, "timestamp", Position{1, 0, 8}},
			Token{BUILTIN, "tolower", Position{2, 0, 6}},
			Token{BUILTIN, "len", Position{3, 0, 2}},
			Token{EOF, "", Position{4, 0, 0}}}},
	{"numeric", "1 23", []Token{
		Token{NUMERIC, "1", Position{0, 0, 0}},
		Token{NUMERIC, "23", Position{0, 2, 3}},
		Token{EOF, "", Position{0, 4, 4}}}},
	{"identifer", "a be foo\nquux line-count", []Token{
		Token{ID, "a", Position{0, 0, 0}},
		Token{ID, "be", Position{0, 2, 3}},
		Token{ID, "foo", Position{0, 5, 7}},
		Token{ID, "quux", Position{1, 0, 3}},
		Token{ID, "line-count", Position{1, 5, 14}},
		Token{EOF, "", Position{1, 15, 15}}}},
	{"regex", "/asdf/", []Token{
		Token{REGEX, "asdf", Position{0, 0, 5}},
		Token{EOF, "", Position{0, 6, 6}}}},
	{"regex with escape", `/asdf\//`, []Token{
		Token{REGEX, `asdf/`, Position{0, 0, 7}},
		Token{EOF, "", Position{0, 8, 8}}}},
	{"regex with escape and special char", `/foo\d\//`, []Token{
		Token{REGEX, `foo\d/`, Position{0, 0, 8}},
		Token{EOF, "", Position{0, 9, 9}}}},
	{"capref", "$foo", []Token{
		Token{CAPREF, "foo", Position{0, 0, 3}},
		Token{EOF, "", Position{0, 4, 4}}}},
	{"numerical capref", "$1", []Token{
		Token{CAPREF, "1", Position{0, 0, 1}},
		Token{EOF, "", Position{0, 2, 2}}}},
	{"capref with trailing punc", "$foo,", []Token{
		Token{CAPREF, "foo", Position{0, 0, 3}},
		Token{COMMA, ",", Position{0, 4, 4}},
		Token{EOF, "", Position{0, 5, 5}}}},
	{"quoted string", `"asdf"`, []Token{
		Token{STRING, `asdf`, Position{0, 0, 5}},
		Token{EOF, "", Position{0, 6, 6}}}},
	{"escaped quote in quoted string", `"\""`, []Token{
		Token{STRING, `"`, Position{0, 0, 3}},
		Token{EOF, "", Position{0, 4, 4}}}},
	{"decorator", `@foo`, []Token{
		Token{DECO, "foo", Position{0, 0, 3}},
		Token{EOF, "", Position{0, 4, 4}}}},
	{"large program",
		"/(?P<date>[[:digit:]-\\/ ])/ {\n" +
			"  strptime($date, \"%Y/%m/%d %H:%M:%S\")\n" +
			"  foo++\n" +
			"}", []Token{
			Token{REGEX, "(?P<date>[[:digit:]-/ ])", Position{0, 0, 26}},
			Token{LCURLY, "{", Position{0, 28, 28}},
			Token{BUILTIN, "strptime", Position{1, 2, 9}},
			Token{LPAREN, "(", Position{1, 10, 10}},
			Token{CAPREF, "date", Position{1, 11, 15}},
			Token{COMMA, ",", Position{1, 16, 16}},
			Token{STRING, "%Y/%m/%d %H:%M:%S", Position{1, 18, 36}},
			Token{RPAREN, ")", Position{1, 37, 37}},
			Token{ID, "foo", Position{2, 2, 4}},
			Token{INC, "++", Position{2, 5, 6}},
			Token{RCURLY, "}", Position{3, 0, 0}},
			Token{EOF, "", Position{3, 1, 1}}}},
	{"linecount",
		"# comment\n" +
			"# blank line\n" +
			"\n" +
			"foo", []Token{
			Token{ID, "foo", Position{3, 0, 2}},
			Token{EOF, "", Position{3, 3, 3}}}},
	// errors
	{"unexpected char", "?", []Token{
		Token{INVALID, "Unexpected input: '?'", Position{0, 0, 0}}}},
	{"unterminated regex", "/foo\n", []Token{
		Token{INVALID, "Unterminated regular expression: \"/foo\"", Position{0, 0, 3}}}},
	{"unterminated quoted string", "\"foo\n", []Token{
		Token{INVALID, "Unterminated quoted string: \"\\\"foo\"", Position{0, 0, 3}}}},
}

// collect gathers the emitted items into a slice.
func collect(t *lexerTest) (tokens []Token) {
	l := NewLexer(t.name, strings.NewReader(t.input))
	for {
		token := l.NextToken()
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
