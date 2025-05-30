// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"sort"
	"strings"
	"unicode"

	"github.com/golang/glog"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/position"
)

// List of keywords.  Keep this list sorted!
var keywords = map[string]Kind{
	"after":     AFTER,
	"as":        AS,
	"buckets":   BUCKETS,
	"by":        BY,
	"const":     CONST,
	"counter":   COUNTER,
	"def":       DEF,
	"del":       DEL,
	"else":      ELSE,
	"gauge":     GAUGE,
	"hidden":    HIDDEN,
	"histogram": HISTOGRAM,
	"limit":     LIMIT,
	"next":      NEXT,
	"otherwise": OTHERWISE,
	"stop":      STOP,
	"text":      TEXT,
	"timer":     TIMER,
}

// List of builtin functions.  Keep this list sorted!
var builtins = []string{
	"bool",
	"float",
	"getfilename",
	"int",
	"len",
	"settime",
	"string",
	"strptime",
	"strtol",
	"subst",
	"timestamp",
	"tolower",
}

// Dictionary returns a list of all keywords and builtins of the language.
func Dictionary() (r []string) {
	for k := range keywords {
		r = append(r, k)
	}
	r = append(r, builtins...)
	return
}

// A stateFn represents each state the scanner can be in.
type stateFn func(*Lexer) stateFn

// A lexer holds the state of the scanner.
type Lexer struct {
	name  string        // Name of program.
	input *bufio.Reader // Source program
	state stateFn       // Current state function of the lexer.

	// The "read cursor" in the input.
	rune  rune // The current rune.
	width int  // Width in bytes.
	line  int  // The line position of the current rune.
	col   int  // The column position of the current rune.

	InRegex bool // Context aware flag from parser to say we're in a regex

	// The currently being lexed token.
	startcol int             // Starting column of the current token.
	text     strings.Builder // the text of the current token

	tokens chan Token // Output channel for tokens emitted.
}

// NewLexer creates a new scanner type that reads the input provided.
func NewLexer(name string, input io.Reader) *Lexer {
	l := &Lexer{
		name:   name,
		input:  bufio.NewReader(input),
		state:  lexProg,
		tokens: make(chan Token, 2),
	}
	return l
}

// NextToken returns the next token in the input.  When no token is available
// to be returned it executes the next action in the state machine.
func (l *Lexer) NextToken() Token {
	for {
		select {
		case tok := <-l.tokens:
			return tok
		default:
			l.state = l.state(l)
		}
	}
}

// emit passes a token to the client.
func (l *Lexer) emit(kind Kind) {
	pos := position.Position{l.name, l.line, l.startcol, l.col - 1}
	glog.V(2).Infof("Emitting %v spelled %q at %v", kind, l.text.String(), pos)
	l.tokens <- Token{kind, l.text.String(), pos}
	// Reset the current token
	l.text.Reset()
	l.startcol = l.col
}

// Internal end of file value.
const eof rune = -1

// next returns the next rune in the input.
func (l *Lexer) next() rune {
	var err error
	l.rune, l.width, err = l.input.ReadRune()
	if errors.Is(err, io.EOF) {
		l.width = 1
		l.rune = eof
	}
	if l.rune == '␤' {
		l.rune = eof
	}
	return l.rune
}

// backup indicates that we haven't yet dealt with the next rune. Use when
// terminating tokens on unknown runes.
func (l *Lexer) backup() {
	l.width = 0
	if l.rune == eof {
		return
	}
	if err := l.input.UnreadRune(); err != nil {
		glog.Info(err)
	}
}

// stepCursor moves the read cursor.
func (l *Lexer) stepCursor() {
	if l.rune == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col += l.width
	}
}

// accept accepts the current rune and its position into the current token.
func (l *Lexer) accept() {
	l.text.WriteRune(l.rune)
	l.stepCursor()
}

// skip does not accept the current rune into the current token's text, but
// does accept its position into the token. Use only at the start or end of a
// token.
func (l *Lexer) skip() {
	l.stepCursor()
}

// ignore skips over the current rune, removing it from the text of the token,
// and resetting the start position of the current token. Use only between
// tokens.
func (l *Lexer) ignore() {
	l.stepCursor()
	l.startcol = l.col
}

// errorf returns an error token and resets the scanner.
func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
	pos := position.Position{
		Filename: l.name,
		Line:     l.line,
		Startcol: l.startcol,
		Endcol:   l.col - 1,
	}
	l.tokens <- Token{
		Kind:     INVALID,
		Spelling: fmt.Sprintf(format, args...),
		Pos:      pos,
	}
	// Reset the current token
	l.text.Reset()
	l.startcol = l.col
	return lexProg
}

// State functions.

// lexProg starts lexing a program.
func lexProg(l *Lexer) stateFn {
	if l.InRegex {
		return lexRegex
	}
	switch r := l.next(); {
	case r == '\n':
		l.accept()
		l.emit(NL)
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
	case r == '[':
		l.accept()
		l.emit(LSQUARE)
	case r == ']':
		l.accept()
		l.emit(RSQUARE)
	case r == ',':
		l.accept()
		l.emit(COMMA)
	case r == '-':
		l.accept()
		switch r = l.next(); {
		case r == '-':
			l.accept()
			l.emit(DEC)
		case isDigit(r):
			l.backup()
			return lexNumeric
		default:
			l.backup()
			l.emit(MINUS)
		}
	case r == '+':
		l.accept()
		switch l.next() {
		case '+':
			l.accept()
			l.emit(INC)
		case '=':
			l.accept()
			l.emit(ADD_ASSIGN)
		default:
			l.backup()
			l.emit(PLUS)
		}
	case r == '*':
		l.accept()
		switch l.next() {
		case '*':
			l.accept()
			l.emit(POW)
		default:
			l.backup()
			l.emit(MUL)
		}
	case r == '=':
		l.accept()
		switch l.next() {
		case '=':
			l.accept()
			l.emit(EQ)
		case '~':
			l.accept()
			l.emit(MATCH)
		default:
			l.backup()
			l.emit(ASSIGN)
		}
	case r == '<':
		l.accept()
		switch l.next() {
		case '=':
			l.accept()
			l.emit(LE)
		case '<':
			l.accept()
			l.emit(SHL)
		default:
			l.backup()
			l.emit(LT)
		}
	case r == '>':
		l.accept()
		switch l.next() {
		case '=':
			l.accept()
			l.emit(GE)
		case '>':
			l.accept()
			l.emit(SHR)
		default:
			l.backup()
			l.emit(GT)
		}
	case r == '!':
		l.accept()
		switch l.next() {
		case '=':
			l.accept()
			l.emit(NE)
		case '~':
			l.accept()
			l.emit(NOT_MATCH)
		default:
			l.backup()
			return l.errorf("Unexpected input: %q", r)
		}
	case r == '/':
		l.accept()
		l.emit(DIV)
	case r == '%':
		l.accept()
		l.emit(MOD)
	case r == '&':
		l.accept()
		switch l.next() {
		case '&':
			l.accept()
			l.emit(AND)
		default:
			l.backup()
			l.emit(BITAND)
		}
	case r == '|':
		l.accept()
		switch l.next() {
		case '|':
			l.accept()
			l.emit(OR)
		default:
			l.backup()
			l.emit(BITOR)
		}
	case r == '^':
		l.accept()
		l.emit(XOR)
	case r == '~':
		l.accept()
		l.emit(NOT)
	case r == '"':
		return lexQuotedString
	case r == '$':
		return lexCapref
	case r == '@':
		return lexDecorator
	case isDigit(r):
		l.backup()
		return lexNumeric
	case isAlpha(r):
		return lexIdentifier
	case r == eof:
		l.skip()
		l.emit(EOF)
		// Stop the machine, we're done.
		return nil
	case r == '.':
		l.backup()
		return lexNumeric
	default:
		l.accept()
		return l.errorf("Unexpected input: %q", r)
	}
	return lexProg
}

// Lex a comment.
func lexComment(l *Lexer) stateFn {
	l.ignore()
Loop:
	for {
		switch l.next() {
		case '\n':
			l.skip()
			fallthrough
		case eof:
			break Loop
		default:
			l.ignore()
		}
	}
	return lexProg
}

// Lex a numerical constant.
func lexNumeric(l *Lexer) stateFn {
	r := l.next()
	for isDigit(r) {
		l.accept()
		r = l.next()
	}
	if r != '.' && r != 'E' && r != 'e' && !isDurationSuffix(r) {
		l.backup()
		l.emit(Kind(INTLITERAL))
		return lexProg
	}
	if r == '.' {
		l.accept()
		r = l.next()
		for isDigit(r) {
			l.accept()
			r = l.next()
		}
	}
	if r == 'e' || r == 'E' {
		l.accept()
		r = l.next()
		if r == '+' || r == '-' {
			l.accept()
			r = l.next()
		}
		for isDigit(r) {
			l.accept()
			r = l.next()
		}
	}
	if isDurationSuffix(r) {
		l.accept()
		return lexDuration
	}
	l.backup()
	l.emit(Kind(FLOATLITERAL))
	return lexProg
}

func isDurationSuffix(r rune) bool {
	switch r {
	case 's', 'm', 'h', 'd':
		return true
	}
	return false
}

func lexDuration(l *Lexer) stateFn {
Loop:
	for {
		switch r := l.next(); {
		case isDigit(r):
			l.accept()
		case r == '.':
			l.accept()
		case r == '-':
			l.accept()
		case r == '+':
			l.accept()
		case isDurationSuffix(r):
			l.accept()
		default:
			l.backup()
			break Loop
		}
	}
	l.emit(DURATIONLITERAL)
	return lexProg
}

// Lex a quoted string.  The text of a quoted string does not include the '"' quotes.
func lexQuotedString(l *Lexer) stateFn {
	l.skip() // Skip leading quote
Loop:
	for {
		switch l.next() {
		case '\\':
			l.skip()
			if r := l.next(); r != eof && r != '\n' {
				if r != '"' {
					l.text.WriteRune('\\')
				}
				l.accept()
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("Unterminated quoted string: \"\\\"%s\"", l.text.String())
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
// capture groups in the preceding regular expression.
func lexCapref(l *Lexer) stateFn {
	l.skip() // Skip the leading $
	named := false
Loop:
	for {
		switch r := l.next(); {
		case isAlnum(r) || r == '_':
			l.accept()
			if !isDigit(r) {
				named = true
			}
		default:
			l.backup()
			break Loop
		}
	}
	if named {
		l.emit(CAPREF_NAMED)
	} else {
		l.emit(CAPREF)
	}
	return lexProg
}

// Lex an identifier, or builtin keyword.
func lexIdentifier(l *Lexer) stateFn {
	l.accept()
Loop:
	for {
		switch r := l.next(); {
		case isAlnum(r) || r == '_':
			l.accept()
		default:
			l.backup()
			break Loop
		}
	}
	if r, ok := keywords[l.text.String()]; ok {
		l.emit(r)
	} else if r := sort.SearchStrings(builtins, l.text.String()); r >= 0 && r < len(builtins) && builtins[r] == l.text.String() {
		l.emit(BUILTIN)
	} else {
		l.emit(ID)
	}
	return lexProg
}

// Lex a regular expression pattern. The text of the regular expression does
// not include the '/' quotes.
func lexRegex(l *Lexer) stateFn {
	// Exit regex mode when leaving this function.
	defer func() {
		glog.V(2).Info("Exiting regex")
		glog.V(2).Infof("Regex at line %d, startcol %d, col %d", l.line, l.startcol, l.col)
		l.InRegex = false
	}()
Loop:
	for {
		switch l.next() {
		case '\\':
			l.skip()
			if r := l.next(); r != eof && r != '\n' {
				if r != '/' {
					l.text.WriteRune('\\')
				}
				l.accept()
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("Unterminated regular expression: \"/%s\"", l.text.String())
		case '/':
			l.backup() // Backup trailing slash on successful parse
			break Loop
		default:
			l.accept()
		}
	}
	l.emit(REGEX)
	return lexProg
}

// Lex a decorator name. These are functiony templatey wrappers around blocks
// of rules.
func lexDecorator(l *Lexer) stateFn {
	l.skip() // Skip the leading @
Loop:
	for {
		switch r := l.next(); {
		case isAlnum(r) || r == '_':
			l.accept()
		default:
			l.backup()
			break Loop
		}
	}
	l.emit(DECO)
	return lexProg
}

// Helper predicates.

// isAlpha reports whether r is an alphabetical rune.
func isAlpha(r rune) bool {
	return unicode.IsLetter(r)
}

// isAlnum reports whether r is an alphanumeric rune.
func isAlnum(r rune) bool {
	return isAlpha(r) || isDigit(r)
}

// isDigit reports whether r is a numerical rune.
func isDigit(r rune) bool {
	return unicode.IsDigit(r)
}

// isSpace reports whether r is whitespace.
func isSpace(r rune) bool {
	return unicode.IsSpace(r)
}
