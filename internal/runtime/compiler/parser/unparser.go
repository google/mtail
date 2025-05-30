// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/runtime/compiler/ast"
)

// Unparser is for converting program syntax trees back to program text.
type Unparser struct {
	pos       int
	output    strings.Builder
	line      strings.Builder
	emitTypes bool
}

func (u *Unparser) indent() {
	u.pos += 2
}

func (u *Unparser) outdent() {
	u.pos -= 2
}

func (u *Unparser) prefix() (s string) {
	for i := 0; i < u.pos; i++ {
		s += " "
	}
	return
}

func (u *Unparser) emit(s string) {
	u.line.WriteString(s)
}

func (u *Unparser) newline() {
	u.output.WriteString(u.prefix())
	u.output.WriteString(u.line.String())
	u.output.WriteString("\n")
	u.line.Reset()
}

// VisitBefore implements the ast.Visitor interface.
func (u *Unparser) VisitBefore(n ast.Node) (ast.Visitor, ast.Node) {
	if u.emitTypes {
		u.emit(fmt.Sprintf("<%s>(", n.Type()))
	}
	switch v := n.(type) {
	case *ast.StmtList:
		for _, child := range v.Children {
			ast.Walk(u, child)
			u.newline()
		}

	case *ast.ExprList:
		if len(v.Children) > 0 {
			ast.Walk(u, v.Children[0])
			for _, child := range v.Children[1:] {
				u.emit(", ")
				ast.Walk(u, child)
			}
		}

	case *ast.CondStmt:
		if v.Cond != nil {
			ast.Walk(u, v.Cond)
		}
		u.emit(" {")
		u.newline()
		u.indent()
		ast.Walk(u, v.Truth)
		if v.Else != nil {
			u.outdent()
			u.emit("} else {")
			u.newline()
			u.indent()
			ast.Walk(u, v.Else)
		}
		u.outdent()
		u.emit("}")

	case *ast.PatternFragment:
		u.emit("const ")
		ast.Walk(u, v.ID)
		u.emit(" ")
		ast.Walk(u, v.Expr)

	case *ast.PatternLit:
		u.emit("/" + strings.ReplaceAll(v.Pattern, "/", "\\/") + "/")

	case *ast.BinaryExpr:
		ast.Walk(u, v.LHS)
		switch v.Op {
		case LT:
			u.emit(" < ")
		case GT:
			u.emit(" > ")
		case LE:
			u.emit(" <= ")
		case GE:
			u.emit(" >= ")
		case EQ:
			u.emit(" == ")
		case NE:
			u.emit(" != ")
		case SHL:
			u.emit(" << ")
		case SHR:
			u.emit(" >> ")
		case BITAND:
			u.emit(" & ")
		case BITOR:
			u.emit(" | ")
		case XOR:
			u.emit(" ^ ")
		case NOT:
			u.emit(" ~ ")
		case AND:
			u.emit(" && ")
		case OR:
			u.emit(" || ")
		case PLUS:
			u.emit(" + ")
		case MINUS:
			u.emit(" - ")
		case MUL:
			u.emit(" * ")
		case DIV:
			u.emit(" / ")
		case POW:
			u.emit(" ** ")
		case ASSIGN:
			u.emit(" = ")
		case ADD_ASSIGN:
			u.emit(" += ")
		case MOD:
			u.emit(" % ")
		case MATCH:
			u.emit(" =~ ")
		case NOT_MATCH:
			u.emit(" !~ ")
		default:
			u.emit(fmt.Sprintf("Unexpected op: %v", v.Op))
		}
		ast.Walk(u, v.RHS)

	case *ast.IDTerm:
		u.emit(v.Name)

	case *ast.CaprefTerm:
		u.emit("$" + v.Name)

	case *ast.BuiltinExpr:
		u.emit(v.Name + "(")
		if v.Args != nil {
			ast.Walk(u, v.Args)
		}
		u.emit(")")

	case *ast.IndexedExpr:
		ast.Walk(u, v.LHS)
		if len(v.Index.(*ast.ExprList).Children) > 0 {
			u.emit("[")
			ast.Walk(u, v.Index)
			u.emit("]")
		}

	case *ast.VarDecl:
		switch v.Kind {
		case metrics.Counter:
			u.emit("counter ")
		case metrics.Gauge:
			u.emit("gauge ")
		case metrics.Timer:
			u.emit("timer ")
		case metrics.Text:
			u.emit("text ")
		case metrics.Histogram:
			u.emit("histogram ")
		}
		u.emit(v.Name)
		if len(v.Keys) > 0 {
			u.emit(" by " + strings.Join(v.Keys, ", "))
		}
		if v.Limit > 0 {
			u.emit(fmt.Sprintf(" limit %d", v.Limit))
		}
		if len(v.Buckets) > 0 {
			buckets := strings.Builder{}
			buckets.WriteString(" buckets ")
			for _, f := range v.Buckets {
				buckets.WriteString(fmt.Sprintf("%f, ", f))
			}
			u.emit(buckets.String()[:buckets.Len()-2])
		}

	case *ast.UnaryExpr:
		switch v.Op {
		case INC:
			ast.Walk(u, v.Expr)
			u.emit("++")
		case DEC:
			ast.Walk(u, v.Expr)
			u.emit("--")
		case NOT:
			u.emit(" ~")
			ast.Walk(u, v.Expr)
		case MATCH:
			ast.Walk(u, v.Expr)
		default:
			u.emit(fmt.Sprintf("Unexpected op: %s", Kind(v.Op)))
		}

	case *ast.StringLit:
		u.emit("\"" + v.Text + "\"")

	case *ast.IntLit:
		u.emit(strconv.FormatInt(v.I, 10))

	case *ast.FloatLit:
		u.emit(strconv.FormatFloat(v.F, 'g', -1, 64))

	case *ast.DecoDecl:
		u.emit(fmt.Sprintf("def %s {", v.Name))
		u.newline()
		u.indent()
		ast.Walk(u, v.Block)
		u.outdent()
		u.emit("}")

	case *ast.DecoStmt:
		u.emit(fmt.Sprintf("@%s {", v.Name))
		u.newline()
		u.indent()
		ast.Walk(u, v.Block)
		u.outdent()
		u.emit("}")

	case *ast.NextStmt:
		u.emit("next")

	case *ast.OtherwiseStmt:
		u.emit("otherwise")

	case *ast.DelStmt:
		u.emit("del ")
		ast.Walk(u, v.N)
		if v.Expiry > 0 {
			u.emit(fmt.Sprintf(" after %s", v.Expiry))
		}
		u.newline()

	case *ast.ConvExpr:
		ast.Walk(u, v.N)

	case *ast.PatternExpr:
		ast.Walk(u, v.Expr)

	case *ast.Error:
		u.emit("// error")
		u.newline()
		u.emit(v.Spelling)

	case *ast.StopStmt:
		u.emit("stop")

	default:
		panic(fmt.Sprintf("unfound undefined type %T", n))
	}
	if u.emitTypes {
		u.emit(")")
	}
	return nil, n
}

// VisitAfter implements the ast.Visitor interface.
func (u *Unparser) VisitAfter(n ast.Node) ast.Node {
	return n
}

// Unparse begins the unparsing of the syntax tree, returning the program text as a single string.
func (u *Unparser) Unparse(n ast.Node) string {
	ast.Walk(u, n)
	return u.output.String()
}
