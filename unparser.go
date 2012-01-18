// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type unparser struct {
	output string
}

func (p *unparser) visitStmtList(s stmtlistNode) {
	for _, child := range s.children {
		child.acceptVisitor(p)
		p.output += "\n"
	}
}

func (p *unparser) visitExprList(e exprlistNode) {
	if len(e.children) > 0 {
		e.children[0].acceptVisitor(p)
		for _, child := range e.children[1:] {
			p.output += ", "
			child.acceptVisitor(p)
		}
	}
}

func (p *unparser) visitCond(c condNode) {
	if c.cond != nil {
		c.cond.acceptVisitor(p)
	}
	p.output += " {\n"
	for _, child := range c.children {
		child.acceptVisitor(p)
	}
	p.output += "}\n"
}

func (p *unparser) visitRegex(r regexNode) {
	p.output += "/" + r.pattern + "/"
}

func (p *unparser) visitString(s stringNode) {
	p.output += "\"" + s.text + "\""
}

func (p *unparser) visitId(i idNode) {
	p.output += i.name
}

func (p *unparser) visitCapref(c caprefNode) {
	p.output += "$" + c.name
}

func (p *unparser) visitBuiltin(b builtinNode) {
	p.output += b.name + "("
	b.args.acceptVisitor(p)
	p.output += ")"
}
