// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"strings"
)

type Unparser struct {
	pos    int
	output string
	line   string
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

func (u *Unparser) unparse(n node) {
	switch v := n.(type) {
	case *stmtlistNode:
		for _, child := range v.children {
			u.unparse(child)
			u.newline()
		}

	case *exprlistNode:
		if len(v.children) > 0 {
			u.unparse(v.children[0])
			for _, child := range v.children[1:] {
				u.emit(", ")
				u.unparse(child)
			}
		}

	case *condNode:
		if v.cond != nil {
			u.unparse(v.cond)
		}
		u.emit(" {")
		u.newline()
		u.indent()
		for _, child := range v.children {
			u.unparse(child)
		}
		u.outdent()
		u.emit("}")

	case *regexNode:
		u.emit("/" + strings.Replace(v.pattern, "/", "\\/", -1) + "/")

	case *relNode:
		u.unparse(v.lhs)
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
		}
		u.unparse(v.rhs)

	case *stringNode:
		u.emit("\"" + v.text + "\"")

	case *idNode:
		u.emit(v.name)

	case *caprefNode:
		u.emit("$" + v.name)

	case *builtinNode:
		u.emit(v.name + "(")
		if v.args != nil {
			u.unparse(v.args)
		}
		u.emit(")")

	case *additiveExprNode:
		u.unparse(v.lhs)
		u.emit(fmt.Sprintf(" %c ", v.op))
		u.unparse(v.rhs)

	case *assignExprNode:
		u.unparse(v.lhs)
		u.emit(" = ")
		u.unparse(v.rhs)

	case *indexedExprNode:
		u.unparse(v.lhs)
		u.emit("[")
		u.unparse(v.index)
		u.emit("]")

	case *declNode:
		switch v.kind {
		case Counter:
			u.emit("counter ")
		case Gauge:
			u.emit("gauge ")
		}
		u.emit(v.name)
		if len(v.keys) > 0 {
			u.emit(" by " + strings.Join(v.keys, ", "))
		}

	case *incExprNode:
		u.unparse(v.lhs)
		u.emit("++")

	case *incByExprNode:
		u.unparse(v.lhs)
		u.emit(" += ")
		u.unparse(v.rhs)

	case *numericExprNode:
		u.emit(fmt.Sprintf("%d", v.value))

	case *defNode:
		u.emit(fmt.Sprintf("def %s {", v.name))
		u.newline()
		u.indent()
		for _, child := range v.children {
			u.unparse(child)
		}
		u.outdent()
		u.emit("}")

	case *decoNode:
		u.emit(fmt.Sprintf("@%s {", v.name))
		u.newline()
		u.indent()
		for _, child := range v.children {
			u.unparse(child)
		}
		u.outdent()
		u.emit("}")

	case *nextNode:
		u.emit("next")

	default:
		panic(fmt.Sprintf("unparser found undefined type %T", n))
	}
}

func (u *Unparser) Unparse(n node) string {
	u.unparse(n)
	return u.output
}
