// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/google/mtail/internal/metrics"
)

// Unparser is for converting program syntax trees back to program text.
type Unparser struct {
	pos       int
	output    string
	line      string
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
	u.line += s
}

func (u *Unparser) newline() {
	u.output += u.prefix() + u.line + "\n"
	u.line = ""
}

// VisitBefore implements the astNode Visitor interface.
func (u *Unparser) VisitBefore(n astNode) (Visitor, astNode) {
	if u.emitTypes {
		u.emit(fmt.Sprintf("<%s>(", n.Type()))
	}
	switch v := n.(type) {
	case *StmtList:
		for _, child := range v.children {
			Walk(u, child)
			u.newline()
		}

	case *ExprList:
		if len(v.children) > 0 {
			Walk(u, v.children[0])
			for _, child := range v.children[1:] {
				u.emit(", ")
				Walk(u, child)
			}
		}

	case *Cond:
		if v.cond != nil {
			Walk(u, v.cond)
		}
		u.emit(" {")
		u.newline()
		u.indent()
		Walk(u, v.truthNode)
		if v.elseNode != nil {
			u.outdent()
			u.emit("} else {")
			u.indent()
			Walk(u, v.elseNode)
		}
		u.outdent()
		u.emit("}")

	case *PatternFragmentDefNode:
		u.emit("const ")
		Walk(u, v.id)
		u.emit(" ")
		Walk(u, v.expr)

	case *PatternConst:
		u.emit("/" + strings.Replace(v.pattern, "/", "\\/", -1) + "/")

	case *BinaryExpr:
		Walk(u, v.lhs)
		switch v.op {
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
		case CONCAT:
			u.emit(" + ")
		case MATCH:
			u.emit(" =~ ")
		case NOT_MATCH:
			u.emit(" !~ ")
		default:
			u.emit(fmt.Sprintf("Unexpected op: %v", v.op))
		}
		Walk(u, v.rhs)

	case *Id:
		u.emit(v.name)

	case *CaprefNode:
		u.emit("$" + v.name)

	case *BuiltinNode:
		u.emit(v.name + "(")
		if v.args != nil {
			Walk(u, v.args)
		}
		u.emit(")")

	case *IndexedExpr:
		Walk(u, v.lhs)
		if len(v.index.(*ExprList).children) > 0 {
			u.emit("[")
			Walk(u, v.index)
			u.emit("]")
		}

	case *DeclNode:
		switch v.kind {
		case metrics.Counter:
			u.emit("counter ")
		case metrics.Gauge:
			u.emit("gauge ")
		case metrics.Timer:
			u.emit("timer ")
		case metrics.Text:
			u.emit("text ")
		}
		u.emit(v.name)
		if len(v.keys) > 0 {
			u.emit(" by " + strings.Join(v.keys, ", "))
		}

	case *UnaryExpr:
		switch v.op {
		case INC:
			Walk(u, v.expr)
			u.emit("++")
		case DEC:
			Walk(u, v.expr)
			u.emit("--")
		case NOT:
			u.emit(" ~")
			Walk(u, v.expr)
		default:
			u.emit(fmt.Sprintf("Unexpected op: %s", lexeme(v.op)))
		}

	case *StringConst:
		u.emit("\"" + v.text + "\"")

	case *IntConst:
		u.emit(strconv.FormatInt(v.i, 10))

	case *FloatConst:
		u.emit(strconv.FormatFloat(v.f, 'g', -1, 64))

	case *DecoDefNode:
		u.emit(fmt.Sprintf("def %s {", v.name))
		u.newline()
		u.indent()
		Walk(u, v.block)
		u.outdent()
		u.emit("}")

	case *DecoNode:
		u.emit(fmt.Sprintf("@%s {", v.name))
		u.newline()
		u.indent()
		Walk(u, v.block)
		u.outdent()
		u.emit("}")

	case *NextNode:
		u.emit("next")

	case *OtherwiseNode:
		u.emit("otherwise")

	case *DelNode:
		u.emit("del ")
		Walk(u, v.n)
		if v.expiry > 0 {
			u.emit(fmt.Sprintf(" after %s", v.expiry))
		}
		u.newline()

	case *ConvNode:
		Walk(u, v.n)

	case *PatternExpr:
		Walk(u, v.expr)

	case *ErrorNode:
		u.emit("// error")
		u.newline()
		u.emit(v.spelling)

	case *StopNode:
		u.emit("stop")

	default:
		panic(fmt.Sprintf("unparser found undefined type %T", n))
	}
	if u.emitTypes {
		u.emit(")")
	}
	return nil, n
}

// VisitAfter implements the astNode Visitor interface.
func (u *Unparser) VisitAfter(n astNode) astNode {
	return n
}

// Unparse begins the unparsing of the syntax tree, returning the program text as a single string.
func (u *Unparser) Unparse(n astNode) string {
	Walk(u, n)
	return u.output
}
