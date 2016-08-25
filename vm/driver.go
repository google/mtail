// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"flag"
	"fmt"
	"io"
	"strconv"
)

func Parse(name string, input io.Reader) (node, error) {
	p := newParser(name, input)
	r := mtailParse(p)
	if r != 0 || p == nil || p.errors != nil {
		return nil, p.errors
	}
	return p.root, nil
}

const EOF = 0

type parser struct {
	name   string
	root   node
	errors ErrorList
	l      *lexer
	t      token    // Most recently lexed token.
	pos    position // Maybe contains the position of the start of a node when the parser is doing preprocessor concatenation.
	endPos position // Maybe contains the position of the end of a node when the parser is doing preprocessor concatenation.
	symtab SymbolTable
	res    map[string]string // Mapping of regex constants to patterns.
}

func newParser(name string, input io.Reader) *parser {
	mtailDebug = *mtailDebugFlag
	return &parser{name: name, l: newLexer(name, input), res: make(map[string]string)}
}

func (p *parser) ErrorP(s string, pos *position) {
	p.errors.Add(pos, s)
}

func (p *parser) Error(s string) {
	p.errors.Add(&p.t.pos, s)
}

func (p *parser) Lex(lval *mtailSymType) int {
	p.t = p.l.nextToken()
	switch p.t.kind {
	case INVALID:
		p.Error(p.t.text)
		return EOF
	case INTLITERAL:
		var err error
		lval.intVal, err = strconv.ParseInt(p.t.text, 10, 64)
		if err != nil {
			p.Error(fmt.Sprintf("bad number '%s': %s", p.t.text, err))
			return INVALID
		}
	case FLOATLITERAL:
		var err error
		lval.floatVal, err = strconv.ParseFloat(p.t.text, 64)
		if err != nil {
			p.Error(fmt.Sprintf("bad number '%s': %s", p.t.text, err))
			return INVALID
		}
	case LT, GT, LE, GE, NE, EQ, SHL, SHR, AND, OR, XOR, NOT, INC, DIV, MUL, MINUS, PLUS, ASSIGN, ADD_ASSIGN, POW:
		lval.op = int(p.t.kind)
	default:
		lval.text = p.t.text
	}
	return int(p.t.kind)
}

func (p *parser) inRegex() {
	p.l.in_regex = true
}

var mtailDebugFlag = flag.Int("mtailDebug", 0, "Set parser debug level.")
