// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bufio"
	"fmt"
	"io"
	"sort"
	"unicode"

	"github.com/golang/glog"
)

// Lexeme enumerates the types of lexical tokens in a mtail program.
type lexeme int

// Printable names for lexemes.
var lexemeName = map[lexeme]string{
	EOF:             "EOF",
	INVALID:         "INVALID",
	LCURLY:          "LCURLY",
	RCURLY:          "RCURLY",
	LPAREN:          "LPAREN",
	RPAREN:          "RPAREN",
	LSQUARE:         "LSQUARE",
	RSQUARE:         "RSQUARE",
	COMMA:           "COMMA",
	INC:             "INC",
	DEC:             "DEC",
	MINUS:           "MINUS",
	PLUS:            "PLUS",
	MUL:             "MUL",
	DIV:             "DIV",
	MOD:             "MOD",
	POW:             "POW",
	SHL:             "SHL",
	SHR:             "SHR",
	BITAND:          "BITAND",
	BITOR:           "BITOR",
	AND:             "AND",
	OR:              "OR",
	ADD_ASSIGN:      "ADD_ASSIGN",
	ASSIGN:          "ASSIGN",
	LT:              "LT",
	GT:              "GT",
	LE:              "LE",
	GE:              "GE",
	EQ:              "EQ",
	NE:              "NE",
	REGEX:           "REGEX",
	ID:              "ID",
	CAPREF:          "CAPREF",
	CAPREF_NAMED:    "CAPREF_NAMED",
	STRING:          "STRING",
	BUILTIN:         "BUILTIN",
	COUNTER:         "COUNTER",
	GAUGE:           "GAUGE",
	TIMER:           "TIMER",
	AS:              "AS",
	BY:              "BY",
	HIDDEN:          "HIDDEN",
	DEF:             "DEF",
	DECO:            "DECO",
	NEXT:            "NEXT",
	CONST:           "CONST",
	OTHERWISE:       "OTHERWISE",
	ELSE:            "ELSE",
	DEL:             "DEL",
	INTLITERAL:      "INTLITERAL",
	FLOATLITERAL:    "FLOATLITERAL",
	DURATIONLITERAL: "DURATIONLITERAL",
	NL:              "NL",
	CONCAT:          "CONCAT",
	MATCH:           "MATCH",
	NOT_MATCH:       "NOT_MATCH",
	TEXT:            "TEXT",
	AFTER:           "AFTER",
}

func (t lexeme) String() string {
	if s, ok := lexemeName[t]; ok {
		return s
	}
	return fmt.Sprintf("token%d", int(t))
}

// List of keywords.  Keep this list sorted!
var keywords = map[string]lexeme{
	"after":     AFTER,
	"as":        AS,
	"by":        BY,
	"const":     CONST,
	"counter":   COUNTER,
	"def":       DEF,
	"del":       DEL,
	"else":      ELSE,
	"gauge":     GAUGE,
	"hidden":    HIDDEN,
	"next":      NEXT,
	"otherwise": OTHERWISE,
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
	"timestamp",
	"tolower",
}

// token describes a lexed token from the input, containing its type, the
// original text of the token, and its position in the input.
type token struct {
	kind lexeme
	text string
	pos  position
}

func (t token) String() string {
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

	inRegex bool // Context aware flag from parser to say we're in a regex

	// The currently being lexed token.
	startcol int    // Starting column of the current token.
	text     string // the text of the current token

	tokens chan token // Output channel for tokens emitted.
}

// newLexer creates a new scanner type that reads the input provided.
func newLexer(name string, input io.Reader) *lexer {
	l := &lexer{
		name:   name,
		input:  bufio.NewReader(input),
		state:  lexProg,
		tokens: make(chan token, 2),
	}
	return l
}

// nextToken returns the next token in the input.  When no token is available
// to be returned it executes the next action in the state machine.
func (l *lexer) nextToken() token {
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
func (l *lexer) emit(kind lexeme) {
	pos := position{l.name, l.line, l.startcol, l.col - 1}
	glog.V(2).Infof("Emitting %v at %v", kind, pos)
	l.tokens <- token{kind, l.text, pos}
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

// ignore skips over the current rune, removing it from the text of the token,
// and resetting the start position of the current token. Use only between
// tokens.
func (l *lexer) ignore() {
	l.stepCursor()
	l.startcol = l.col
}

// errorf returns an error token and resets the scanner
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	pos := position{l.name, l.line, l.startcol, l.col - 1}
	l.tokens <- token{kind: INVALID,
		text: fmt.Sprintf(format, args...),
		pos:  pos}
	// Reset the current token
	l.text = ""
	l.startcol = l.col
	return lexProg
}

// State functions.

// lexProg starts lexing a program.
func lexProg(l *lexer) stateFn {
	if l.inRegex {
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
func lexComment(l *lexer) stateFn {
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
func lexNumeric(l *lexer) stateFn {
	kind := INTLITERAL
Loop:
	for {
		switch r := l.next(); {
		case isDigit(r):
			l.accept()
		case r == '.':
			if kind == FLOATLITERAL {
				l.backup()
				break Loop
			}
			kind = FLOATLITERAL
			l.accept()
		case isDurationSuffix(r):
			kind = DURATIONLITERAL
			l.accept()
			return lexDuration
		default:
			l.backup()
			break Loop
		}
	}
	l.emit(lexeme(kind))
	return lexProg
}

func isDurationSuffix(r rune) bool {
	switch r {
	case 's', 'm', 'h', 'd':
		return true
	}
	return false
}

func lexDuration(l *lexer) stateFn {
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
func lexQuotedString(l *lexer) stateFn {
	l.skip() // Skip leading quote
Loop:
	for {
		switch l.next() {
		case '\\':
			l.skip()
			if r := l.next(); r != eof && r != '\n' {
				if r != '"' {
					l.text += "\\"
				}
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
// capture groups in the preceding regular expression.
func lexCapref(l *lexer) stateFn {
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
func lexIdentifier(l *lexer) stateFn {
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
	if r, ok := keywords[l.text]; ok {
		l.emit(r)
	} else if r := sort.SearchStrings(builtins, l.text); r >= 0 && r < len(builtins) && builtins[r] == l.text {
		l.emit(BUILTIN)
	} else {
		l.emit(ID)
	}
	return lexProg

}

// Lex a regular expression pattern. The text of the regular expression does
// not include the '/' quotes.
func lexRegex(l *lexer) stateFn {
	// Exit regex mode when leaving this function.
	defer func() {
		glog.V(2).Info("Exiting regex")
		glog.V(2).Infof("Regex at line %d, startcol %d, col %d", l.line, l.startcol, l.col)
		l.inRegex = false
	}()
Loop:
	for {
		switch l.next() {
		case '\\':
			l.skip()
			if r := l.next(); r != eof && r != '\n' {
				if r != '/' {
					l.text += `\`
				}
				l.accept()
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("Unterminated regular expression: \"/%s\"", l.text)
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
func lexDecorator(l *lexer) stateFn {
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
