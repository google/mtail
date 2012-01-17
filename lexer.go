// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

/*
emtail is a metric exporting, modular tail.

This file contains a lexical scanner for the emtail language.
*/
package main

import (
	"bufio"
	"fmt"
	"io"
	"sort"
	"unicode"
)

type Lexeme int

// Printable names for lexemes.
var lexemeName = map[Lexeme]string{
	EOF:     "EOF",
	INVALID: "INVALID",
	LCURLY:  "LCURLY",
	RCURLY:  "RCURLY",
	LPAREN:  "LPAREN",
	RPAREN:  "RPAREN",
	COMMA:   "COMMA",
	REGEX:   "REGEX",
	ID:      "ID",
	CAPREF:  "CAPREF",
	STRING:  "STRING",
	BUILTIN: "BUILTIN",
}

func (t Lexeme) String() string {
	if s, ok := lexemeName[t]; ok {
		return s
	}
	return fmt.Sprintf("token%d", int(t))
}

// List of builtin functions.  Keep this list sorted!
var builtins = []string{
	"inc",
	"strptime",
	"tag",
}

// A Position is the location in the source program that a token appears.
type Position struct {
	line     int // Line in the source for this token.
	startcol int // Starting and ending columns in the source for this token.
	endcol   int
}

func (p Position) String() string {
	r := fmt.Sprintf("%d:%d", p.line+1, p.startcol+1)
	if p.endcol > p.startcol {
		r += fmt.Sprintf("-%d", p.endcol+1)
	}
	return r
}

type Token struct {
	kind Lexeme
	text string
	pos  Position
}

func (t Token) String() string {
	return fmt.Sprintf("%s(%q,%s)", t.kind, t.text, t.pos)
}

// A stateFn represents each state the scanner can be in.
type stateFn func(*lexer) stateFn

// A lexer holds the state of the scanner.
type lexer struct {
	name  string        // Name of program.
	input *bufio.Reader // Source program
	state stateFn       // Current state function of the lexer.

	// The "read cursor" in the input.
	rune  rune // The current rune.
	width int  // Width in bytes.
	line  int  // The line position of the current rune.
	col   int  // The column position of the current rune.

	// The currently being lexed token.
	startcol int    // Starting column of the current token.
	text     string // the text of the current token

	tokens chan Token // Output channel for tokens emitted.
}

// NewLexer creates a new scanner type that reads the input provided.
func NewLexer(name string, input io.Reader) *lexer {
	l := &lexer{
		name:   name,
		input:  bufio.NewReader(input),
		state:  lexProg,
		tokens: make(chan Token, 2),
	}
	return l
}

// NextToken returns the next token in the input.
func (l *lexer) NextToken() Token {
	for {
		select {
		case token := <-l.tokens:
			return token
		default:
			l.state = l.state(l)
		}
	}
	panic("not reached")
}

// emit passes a token to the client.
func (l *lexer) emit(kind Lexeme) {
	pos := Position{l.line, l.startcol, l.col - 1}
	l.tokens <- Token{kind, l.text, pos}
	// Reset the current token
	l.text = ""
	l.startcol = l.col
}

// Internal end of file value.
var eof rune = -1

// next returns the next rune in the input.
func (l *lexer) next() rune {
	var err error
	l.rune, l.width, err = l.input.ReadRune()
	if err == io.EOF {
		l.width = 1
		return eof
	}
	return l.rune
}

// backup indicates that we haven't yet dealt with the next rune. Use when
// terminating tokens on unknown runes.
func (l *lexer) backup() {
	l.input.UnreadRune()
	l.width = 0
}

// stepCursor moves the read cursor.
func (l *lexer) stepCursor() {
	if l.rune == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col += l.width
	}
}

// accept accepts the current rune and its position into the current token.
func (l *lexer) accept() {
	l.text += string(l.rune)
	l.stepCursor()
}

// skip does not accept the current rune into the current token's text, but
// does accept its position into the token. Use only at the start or end of a
// token.
func (l *lexer) skip() {
	l.stepCursor()
}

// ignore skips over the current rune, removing it from the text of the
// token, and resetting the start position of the current token. Use only between
// tokens.
func (l *lexer) ignore() {
	l.stepCursor()
	l.startcol = l.col
}

// errorf returns an error token and terminates the scanner by passing back a
// nil state function to the state machine.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	pos := Position{l.line, l.startcol, l.col - 1}
	l.tokens <- Token{kind: INVALID, text: fmt.Sprintf(format, args...), pos: pos}
	return nil
}

// State functions.

// Start lexing a program.
func lexProg(l *lexer) stateFn {
	switch r := l.next(); {
	case r == '#':
		return lexComment
	case isSpace(r):
		l.ignore()
	case r == '{':
		l.accept()
		l.emit(LCURLY)
	case r == '}':
		l.accept()
		l.emit(RCURLY)
	case r == '(':
		l.accept()
		l.emit(LPAREN)
	case r == ')':
		l.accept()
		l.emit(RPAREN)
	case r == ',':
		l.accept()
		l.emit(COMMA)
	case r == '/':
		return lexRegex
	case r == '"':
		return lexQuotedString
	case r == '$':
		return lexCapref
	case isAlpha(r):
		return lexIdentifier
	case r == eof:
		l.skip()
		l.emit(EOF)
		// Stop the machine, we're done.
		return nil
	default:
		l.accept()
		l.errorf("Unexpected input: %q", r)
		return nil
	}
	return lexProg
}

// Lex a comment.
func lexComment(l *lexer) stateFn {
	l.ignore()
Loop:
	for {
		switch l.next() {
		case eof, '\n':
			break Loop
		default:
			l.ignore()
		}
	}
	return lexProg
}

// Lex a quoted string.  The text of a quoted string does not include the '"' quotes.
func lexQuotedString(l *lexer) stateFn {
	l.skip() // Skip leading quote
Loop:
	for {
		switch l.next() {
		case '\\':
			l.accept()
			if r := l.next(); r != eof && r != '\n' {
				l.accept()
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("Unterminated quoted string: \"\\\"%s\"", l.text)
		case '"':
			l.skip() // Skip trailing quote.
			break Loop
		default:
			l.accept()
		}
	}
	l.emit(STRING)
	return lexProg
}

// Lex a capture group reference. These are local variable references to
// capture groups in the preceeding regular expression.
func lexCapref(l *lexer) stateFn {
	l.skip() // Skip the leading $
Loop:
	for {
		switch r := l.next(); {
		case isAlnum(r):
			l.accept()
		default:
			l.backup()
			break Loop
		}
	}
	l.emit(CAPREF)
	return lexProg
}

// Lex an identifier, or builtin keyword.
func lexIdentifier(l *lexer) stateFn {
	l.accept()
Loop:
	for {
		switch r := l.next(); {
		case isAlnum(r) || r == '-' || r == '_':
			l.accept()
		default:
			l.backup()
			break Loop
		}
	}
	if r := sort.SearchStrings(builtins, l.text); r >= 0 && r < len(builtins) && builtins[r] == l.text {
		l.emit(BUILTIN)
	} else {
		l.emit(ID)
	}
	return lexProg

}

// Lex a regular expression. The text of the regular expression does not
// include the '/' quotes.
func lexRegex(l *lexer) stateFn {
	l.skip() // Skip leading quote
Loop:
	for {
		switch l.next() {
		case '\\':
			l.accept()
			if r := l.next(); r != eof && r != '\n' {
				l.accept()
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("Unterminated regular expression: \"/%s\"", l.text)
		case '/':
			l.skip() // Skip trailing quote
			break Loop
		default:
			l.accept()
		}
	}
	l.emit(REGEX)
	return lexProg
}

// Helper predicates.

// isAlpha reports whether r is an alphabetical rune.
func isAlpha(r rune) bool {
	return unicode.IsLetter(r)
}

// isAlnum reports whether r is an alphanumeric rune.
func isAlnum(r rune) bool {
	return isAlpha(r) || unicode.IsDigit(r)
}

// isSpace reports whether r is whitespace.
func isSpace(r rune) bool {
	return unicode.IsSpace(r)
}
