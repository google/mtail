// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/google/mtail/internal/metrics"
)

// Sexp is for converting program syntax trees into typed s-expression for printing
type Sexp struct {
	output string // Accumulator for the result

	emitTypes bool

	col  int // column to indent current line to
	line string
}

func (s *Sexp) indent() {
	s.col += 2
}

func (s *Sexp) outdent() {
	s.col -= 2
}

func (s *Sexp) prefix() (r string) {
	for i := 0; i < s.col; i++ {
		r += " "
	}
	return
}

func (s *Sexp) emit(str string) {
	s.line += str
}

func (s *Sexp) newline() {
	if s.line != "" {
		s.output += s.prefix() + s.line
	}
	s.output += "\n"
	s.line = ""
}

// VisitBefore implements the astNode Visitor interface.
func (s *Sexp) VisitBefore(n astNode) (Visitor, astNode) {
	s.emit(fmt.Sprintf("( ;;%T ", n))
	if s.emitTypes {
		s.emit(fmt.Sprintf("<%s> ", n.Type()))
	}
	s.emit(fmt.Sprintf("@ %s", n.Pos()))
	s.newline()
	s.indent()
	switch v := n.(type) {

	case *PatternFragmentDefNode:
		s.emit("const ")
		n = Walk(s, v.id)
		s.emit(" ")

	case *PatternConst:
		s.emit(fmt.Sprintf("%q", v.pattern))

	case *BinaryExpr:
		switch v.op {
		case LT:
			s.emit("<")
		case GT:
			s.emit(">")
		case LE:
			s.emit("<=")
		case GE:
			s.emit(">=")
		case EQ:
			s.emit("==")
		case NE:
			s.emit("!=")
		case SHL:
			s.emit("<<")
		case SHR:
			s.emit(">>")
		case BITAND:
			s.emit("&")
		case BITOR:
			s.emit("|")
		case XOR:
			s.emit("^")
		case NOT:
			s.emit("~")
		case AND:
			s.emit("&&")
		case OR:
			s.emit("||")
		case PLUS:
			s.emit("+")
		case MINUS:
			s.emit("-")
		case MUL:
			s.emit("*")
		case DIV:
			s.emit("/")
		case POW:
			s.emit("**")
		case ASSIGN:
			s.emit("=")
		case ADD_ASSIGN:
			s.emit("+=")
		case MOD:
			s.emit("%")
		case CONCAT:
			s.emit("++")
		case MATCH:
			s.emit("=~")
		case NOT_MATCH:
			s.emit("!~")
		default:
			s.emit(fmt.Sprintf("Unexpected op: %s", lexeme(v.op)))
		}
		s.newline()
		s.indent()

	case *Id:
		s.emit("\"" + v.name + "\"")

	case *CaprefNode:
		s.emit("\"" + v.name + "\"")

	case *BuiltinNode:
		s.emit("\"" + v.name + "\"")
		s.newline()

	case *DeclNode:
		switch v.kind {
		case metrics.Counter:
			s.emit("counter ")
		case metrics.Gauge:
			s.emit("gauge ")
		case metrics.Timer:
			s.emit("timer ")
		case metrics.Text:
			s.emit("text ")
		}
		s.emit(v.name)
		if len(v.keys) > 0 {
			s.emit(" (")
			s.emit(strings.Join(v.keys, " "))
			s.emit(")")
		}

	case *UnaryExpr:
		switch v.op {
		case INC:
			s.emit("++")
		case DEC:
			s.emit("--")
		case NOT:
			s.emit("~")
		default:
			s.emit(fmt.Sprintf("Unexpected op: %s", lexeme(v.op)))
		}
		s.newline()
		s.indent()

	case *StringConst:
		s.emit("\"" + v.text + "\"")

	case *IntConst:
		s.emit(strconv.FormatInt(v.i, 10))

	case *FloatConst:
		s.emit(strconv.FormatFloat(v.f, 'g', -1, 64))

	case *NextNode:
		s.emit("next")
	case *OtherwiseNode:
		s.emit("otherwise")
	case *DelNode:
		s.emit("del")
		if v.expiry > 0 {
			s.emit(fmt.Sprintf(" after %s", v.expiry))
		}

	case *ConvNode:
		s.emit("conv")

	case *ErrorNode:
		s.emit(fmt.Sprintf("error %q", v.spelling))

	case *StopNode:
		s.emit("stop")

	case *IndexedExpr, *StmtList, *ExprList, *Cond, *DecoDefNode, *DecoNode, *PatternExpr: // normal walk

	default:
		panic(fmt.Sprintf("sexp found undefined type %T", n))
	}
	return s, n
}

// VisitAfter implements the astNode Visitor interface.
func (s *Sexp) VisitAfter(node astNode) astNode {
	switch node.(type) {
	case *BinaryExpr:
		s.outdent()
	}
	s.outdent()
	s.emit(")")
	s.newline()
	return node
}

// Dump begins the dumping of the syntax tree, returning the s-expression as a single string
func (s *Sexp) Dump(n astNode) string {
	Walk(s, n)
	return s.output
}
