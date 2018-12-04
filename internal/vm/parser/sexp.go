// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/vm/ast"
)

// Sexp is for converting program syntax trees into typed s-expression for printing
type Sexp struct {
	output string // Accumulator for the result

	EmitTypes bool

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
func (s *Sexp) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	s.emit(fmt.Sprintf("( ;;%T ", n))
	if s.EmitTypes {
		s.emit(fmt.Sprintf("<%s> ", n.Type()))
	}
	s.emit(fmt.Sprintf("@ %s", n.Pos()))
	s.newline()
	s.indent()
	switch v := n.(type) {

	case *ast.PatternFragment:
		s.emit("const ")
		n = ast.Walk(s, v.Id)
		s.emit(" ")

	case *ast.PatternLit:
		s.emit(fmt.Sprintf("%q", v.Pattern))

	case *ast.BinaryExpr:
		switch v.Op {
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
			s.emit(fmt.Sprintf("Unexpected op: %s", Kind(v.Op)))
		}
		s.newline()
		s.indent()

	case *ast.IdTerm:
		s.emit("\"" + v.Name + "\"")

	case *ast.CaprefTerm:
		s.emit("\"" + v.Name + "\"")

	case *ast.BuiltinExpr:
		s.emit("\"" + v.Name + "\"")
		s.newline()

	case *ast.VarDecl:
		switch v.Kind {
		case metrics.Counter:
			s.emit("counter ")
		case metrics.Gauge:
			s.emit("gauge ")
		case metrics.Timer:
			s.emit("timer ")
		case metrics.Text:
			s.emit("text ")
		}
		s.emit(v.Name)
		if len(v.Keys) > 0 {
			s.emit(" (")
			s.emit(strings.Join(v.Keys, " "))
			s.emit(")")
		}

	case *ast.UnaryExpr:
		switch v.Op {
		case INC:
			s.emit("++")
		case DEC:
			s.emit("--")
		case NOT:
			s.emit("~")
		default:
			s.emit(fmt.Sprintf("Unexpected op: %s", Kind(v.Op)))
		}
		s.newline()
		s.indent()

	case *ast.StringLit:
		s.emit("\"" + v.Text + "\"")

	case *ast.IntLit:
		s.emit(strconv.FormatInt(v.I, 10))

	case *ast.FloatLit:
		s.emit(strconv.FormatFloat(v.F, 'g', -1, 64))

	case *ast.NextStmt:
		s.emit("next")
	case *ast.OtherwiseStmt:
		s.emit("otherwise")
	case *ast.DelStmt:
		s.emit("del")
		if v.Expiry > 0 {
			s.emit(fmt.Sprintf(" after %s", v.Expiry))
		}

	case *ast.ConvExpr:
		s.emit("conv")

	case *ast.Error:
		s.emit(fmt.Sprintf("error %q", v.Spelling))

	case *ast.StopStmt:
		s.emit("stop")

	case *ast.IndexedExpr, *ast.StmtList, *ast.ExprList, *ast.CondStmt, *ast.DecoDecl, *ast.DecoStmt, *ast.PatternExpr: // normal walk

	default:
		panic(fmt.Sprintf("sexp found undefined type %T", n))
	}
	return s, n
}

// VisitAfter implements the astNode Visitor interface.
func (s *Sexp) VisitAfter(node ast.Node) ast.Node {
	switch node.(type) {
	case *ast.BinaryExpr:
		s.outdent()
	}
	s.outdent()
	s.emit(")")
	s.newline()
	return node
}

// Dump begins the dumping of the syntax tree, returning the s-expression as a single string
func (s *Sexp) Dump(n ast.Node) string {
	ast.Walk(s, n)
	return s.output
}
