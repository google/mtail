// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
        "fmt"
        "strings"
)

type asm struct {
        label    int
        assembly []string
        args     []string
}

func (a *asm) emit(s string) {
        a.assembly = append(a.assembly, s)
}

func (a *asm) getlabel() string {
        r := fmt.Sprintf("L%d", a.label)
        a.label++
        return r
}

func (a *asm) visitStmtList(s stmtlistNode) {
        for _, child := range s.children {
                child.acceptVisitor(a)
        }
}

func (a *asm) visitExprList(e exprlistNode) {
        for _, child := range e.children {
                child.acceptVisitor(a)
        }
}

func (a *asm) visitCond(c condNode) {
        if c.cond != nil {
                c.cond.acceptVisitor(a)
        }
        label := a.getlabel()
        a.emit("jnm " + label)
        for _, child := range c.children {
                child.acceptVisitor(a)
        }
        a.emit("ret true")
        a.emit(label + ":")
}

func (a *asm) visitRegex(r regexNode) {
        a.emit("match /" + r.pattern + "/")
}

func (a *asm) visitString(s stringNode) {
        a.args = append(a.args, s.text)
}

func (a *asm) visitId(i idNode) {
        a.args = append(a.args, i.name)
}

func (a *asm) visitCapref(c caprefNode) {
        a.args = append(a.args, c.name)
}

func (a *asm) visitBuiltin(b builtinNode) {
        a.args = make([]string, 0)
        for _, child := range b.children {
                child.acceptVisitor(a)
        }
        args := strings.Join(a.args, " ")
        a.emit(fmt.Sprintf("%s %s", b.name, args))
}
