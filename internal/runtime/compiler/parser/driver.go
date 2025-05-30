// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Build the parser:
//go:generate goyacc -v y.output -o parser.go -p mtail parser.y

// Package parser implements the parse phase of the mtail program compilation.
// The parser itself is defined in parser.y, and goyacc generates the program
// code and token definitions.  The parser fetches tokens from the lexer, which
// scans the input converting the program source into a token stream.  The
// driver code wraps the generated parser and marshals the ast and errors back
// to the caller.
//
// Two pretty-printers are used for debugging: the unparser, which converts an
// ast back into program text, and an approximation of an s-expression printer,
// which tries to model in indented text the structure of the ast.
package parser

import (
	"flag"
	"fmt"
	"io"
	"strconv"
	"time"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/errors"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
)

// Parse reads the program named name from the input, and if successful returns
// an ast.Node for the root of the AST, otherwise parser errors.
func Parse(name string, input io.Reader) (ast.Node, error) {
	p := newParser(name, input)
	r := mtailParse(p)
	if r != 0 || p.errors != nil {
		return nil, &p.errors
	}
	return p.root, nil
}

// EOF is a marker for end of file.  It has the same value as the goyacc internal Kind `$end`.
const EOF = 0

// parser defines the data structure for parsing an mtail program.
type parser struct {
	name   string
	root   ast.Node
	errors errors.ErrorList
	l      *Lexer
	t      Token             // Most recently lexed token.
	pos    position.Position // Optionally contains the position of the start of a production
}

func newParser(name string, input io.Reader) *parser {
	return &parser{name: name, l: NewLexer(name, input)}
}

func (p *parser) ErrorP(s string, pos *position.Position) {
	p.errors.Add(pos, s)
}

func (p *parser) Error(s string) {
	p.errors.Add(&p.t.Pos, s)
}

// Lex reads the next token from the Lexer, turning it into a form useful for the goyacc generated parser.
// The variable lval is modified to carry token information, and the token type is returned.
func (p *parser) Lex(lval *mtailSymType) int {
	p.t = p.l.NextToken()
	switch p.t.Kind {
	case INVALID:
		p.Error(p.t.Spelling)
		lval.text = p.t.Spelling
		return INVALID
	case INTLITERAL:
		var err error
		lval.intVal, err = strconv.ParseInt(p.t.Spelling, 10, 64)
		if err != nil {
			p.Error(fmt.Sprintf("bad number '%s': %s", p.t.Spelling, err))
			return INVALID
		}
	case FLOATLITERAL:
		var err error
		lval.floatVal, err = strconv.ParseFloat(p.t.Spelling, 64)
		if err != nil {
			p.Error(fmt.Sprintf("bad number '%s': %s", p.t.Spelling, err))
			return INVALID
		}
	case DURATIONLITERAL:
		var err error
		lval.duration, err = time.ParseDuration(p.t.Spelling)
		if err != nil {
			p.Error(fmt.Sprintf("%s", err))
			return INVALID
		}
	case LT, GT, LE, GE, NE, EQ, SHL, SHR, BITAND, BITOR, AND, OR, XOR, NOT, INC, DEC, DIV, MUL, MINUS, PLUS, ASSIGN, ADD_ASSIGN, POW, MOD, MATCH, NOT_MATCH:
		lval.op = int(p.t.Kind)
	default:
		lval.text = p.t.Spelling
	}
	return int(p.t.Kind)
}

func (p *parser) inRegex() {
	glog.V(2).Info("Entering regex")
	p.l.InRegex = true
}

func init() {
	// Initialise globals defined in generated parser.go, defaults to 0 and false
	flag.IntVar(&mtailDebug, "mtailDebug", 0, "Set parser debug level.")
	mtailErrorVerbose = true
}
