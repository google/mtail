// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"strings"
)

func unparse(n node) string {
	output := ""

	switch v := n.(type) {
	case *stmtlistNode:
		for _, child := range v.children {
			output += unparse(child) + "\n"
		}

	case *exprlistNode:
		if len(v.children) > 0 {
			output += unparse(v.children[0])
			for _, child := range v.children[1:] {
				output += ", " + unparse(child)
			}
		}

	case *condNode:
		if v.cond != nil {
			output += unparse(v.cond)
		}
		output += " {\n"
		for _, child := range v.children {
			output += unparse(child)
		}
		output += "}\n"

	case *regexNode:
		output += "/" + strings.Replace(v.pattern, "/", "\\/", -1) + "/"

	case *relNode:
		output += unparse(v.lhs)
		switch v.op {
		case LT:
			output += " < "
		case GT:
			output += " > "
		case LE:
			output += " <= "
		case GE:
			output += " >= "
		case EQ:
			output += " == "
		case NE:
			output += " != "
		}
		output += unparse(v.rhs)

	case *stringNode:
		output += "\"" + v.text + "\""

	case *idNode:
		output += v.name

	case *caprefNode:
		output += "$" + v.name

	case *builtinNode:
		output += v.name + "("
		if v.args != nil {
			output += unparse(v.args)
		}
		output += ")"

	case *additiveExprNode:
		output += unparse(v.lhs)
		output += fmt.Sprintf(" %c ", v.op)
		output += unparse(v.rhs)

	case *assignExprNode:
		output += unparse(v.lhs)
		output += " = "
		output += unparse(v.rhs)

	case *indexedExprNode:
		output += unparse(v.lhs)
		output += "["
		output += unparse(v.index)
		output += "]"

	case *declNode:
		switch v.kind {
		case Counter:
			output += "counter "
		case Gauge:
			output += "gauge "
		}
		output += v.name

	case *incExprNode:
		output += unparse(v.lhs)
		output += "++"

	case *incByExprNode:
		output += unparse(v.lhs)
		output += " += "
		output += unparse(v.rhs)

	case *constExprNode:
		output += fmt.Sprintf("%d", v.value)

	case *defNode:
		output += fmt.Sprintf("def %s {\n", v.name)
		for _, child := range v.children {
			output += unparse(child)
		}
		output += "}\n"

	case *decoNode:
		output += fmt.Sprintf("@%s {\n", v.name)
		for _, child := range v.children {
			output += unparse(child)
		}
		output += "}\n"

	case *nextNode:
		output += "next\n"

	default:
		panic(fmt.Sprintf("unparser found undefined type %T", n))
	}
	return output
}
