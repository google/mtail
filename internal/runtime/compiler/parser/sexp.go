// Copyright 2017 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/symbol"
)

// Sexp is for converting program syntax trees into typed s-expression for printing.
type Sexp struct {
	output strings.Builder // Accumulator for the result

	EmitTypes bool

	col  int // column to indent current line to
	line strings.Builder
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
	s.line.WriteString(str)
}

func (s *Sexp) newline() {
	if s.line.Len() > 0 {
		s.output.WriteString(s.prefix())
		s.output.WriteString(s.line.String())
	}
	s.output.WriteString("\n")
	s.line.Reset()
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
		ast.Walk(s, v.ID)
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
		case MATCH:
			s.emit("=~")
		case NOT_MATCH:
			s.emit("!~")
		default:
			s.emit(fmt.Sprintf("Unexpected op: %s", Kind(v.Op)))
		}
		s.newline()

	case *ast.IDTerm:
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
			s.emit("increment")
		case DEC:
			s.emit("decrement")
		case NOT:
			s.emit("unary-not")
		case MATCH:
			s.emit("match")
		default:
			s.emit(fmt.Sprintf("Unexpected op: %s", Kind(v.Op)))
		}
		s.newline()

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

	case *ast.DecoDecl:
		s.emit(fmt.Sprintf("%q", v.Name))
		s.newline()
		s.emitScope(v.Scope)

	case *ast.DecoStmt:
		s.emit(fmt.Sprintf("%q", v.Name))
		s.newline()

	case *ast.StmtList:
		s.emitScope(v.Scope)

	case *ast.CondStmt:
		s.emitScope(v.Scope)

	case *ast.IndexedExpr, *ast.ExprList, *ast.PatternExpr: // normal walk

	default:
		panic(fmt.Sprintf("sexp found undefined type %T", n))
	}
	return s, n
}

// VisitAfter implements the astNode Visitor interface.
func (s *Sexp) VisitAfter(node ast.Node) ast.Node {
	s.outdent()
	s.emit(")")
	s.newline()
	return node
}

func (s *Sexp) emitScope(scope *symbol.Scope) {
	s.emit(fmt.Sprintf("Scope: %p (", scope))
	s.newline()
	if scope != nil {
		if scope.Parent != nil {
			s.indent()
			s.emit(fmt.Sprintf("Parent: %p", scope.Parent))
			s.newline()
			s.outdent()
		}
		if len(scope.Symbols) > 0 {
			s.indent()
			for name, sym := range scope.Symbols {
				s.emit(fmt.Sprintf("%q: %v %q %v", name, sym.Kind, sym.Name, sym.Used))
				s.newline()
			}
			s.outdent()
		}
	}
	s.emit(")")
	s.newline()
}

// Dump begins the dumping of the syntax tree, returning the s-expression as a single string.
func (s *Sexp) Dump(n ast.Node) string {
	s.output.Reset()
	ast.Walk(s, n)
	return s.output.String()
}
