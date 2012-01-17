// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
        "fmt"
        "io"
        "log"
        "regexp"
        "strconv"
        "time"
)

type opcode int

const (
        match   opcode  = iota
        jnm
        inc
        strptime
        label
        ret
        push
        capref
        load
)

var opNames = map[opcode]string{
        match:    "match",
        jnm:      "jnm",
        inc:      "inc",
        strptime: "strptime",
        label:    "label",
        ret:      "ret",
        push:     "push",
        capref:   "capref",
        load:     "load",
}

var builtin = map[string]opcode{
        "inc":      inc,
        "strptime": strptime,
        "label":    label,
}

type instr struct {
        op      opcode
        opnd    int
}

func (i instr) String() string {
        return fmt.Sprintf("%s %d", opNames[i.op], i.opnd)
}

type thread struct {
        pc      int
        reg     int
        matches []string
        time    time.Time
        labels  map[string]string
}

type vm struct {
        prog    []instr
        re      []*regexp.Regexp

        str     []string

        stack   []interface{}
}

func (v *vm) pop() interface{} {
        m := v.stack[len(v.stack)-1]
        v.stack = v.stack[:len(v.stack)-1]
        return m
}

func (v *vm) push(opnd interface{}) {
        v.stack = append(v.stack, opnd)
}

func (v *vm) Run(input string) bool {
        // Create a first thread to execute.
        t := new(thread)
        for {
                // fetch
                if t.pc >= len(v.prog) {
                        return false
                }
                i := v.prog[t.pc]

                // execute
                switch i.op {
                case match:
                        t.labels = map[string]string{}
                        // match regex and stre success
                        t.matches = v.re[i.opnd].FindStringSubmatch(input)
                        if t.matches != nil {
                                t.reg = 1
                        } else {
                                t.reg = 0
                        }
                case jnm:
                        if t.reg == 0 {
                                t.pc = i.opnd
                                continue
                        }
                case inc:
                        // increment a counter
                        m := v.pop().(int)
                        metrics[m].value++
                        metrics[m].time = t.time
                case strptime:
                        layout := v.pop().(string)
                        s := v.pop().(string)
                        tm, err := time.Parse(layout, s)
                        if err != nil {
                                log.Println("time parse:", err)
                                break
                        }
                        t.time = tm
                case label:
                        val := v.pop().(string)
                        key := v.pop().(string)
                        t.labels[key] = val
                case capref:
                        v.push(t.matches[i.opnd])
                case load:
                        v.push(v.str[i.opnd])
                case ret:
                        // reg is an int 1 or 0 for true/false
                        return i.opnd == 1
                case push:
                        v.push(i.opnd)
                default:
                        log.Println("illegal instruction", i.op)
                        return false
                }
                t.pc++
        }
        panic("not reached")
}

type compiler struct {
        prog    []instr
        str     []string
        re      []*regexp.Regexp

        metrics map[string]int
        refs    []int
}

func Compile(name string, input io.Reader) (*vm, []string) {
        p := NewParser(name, input)
        EmtailParse(p)
        if p == nil || len(p.errors) > 0 {
                return nil, p.errors
        }
        c := &compiler{}
        p.root.acceptVisitor(c)
        vm := &vm{
                re:     c.re,
                str:    c.str,
                prog:   c.prog}
        return vm, nil
}

func (c *compiler) emit(i instr) {
        c.prog = append(c.prog, i)
}

func (c *compiler) visitStmtList(n stmtlistNode) {
        for _, child := range n.children {
                child.acceptVisitor(c)
        }
}

func (c *compiler) visitExprList(n exprlistNode) {
        for _, child := range n.children {
                child.acceptVisitor(c)
        }
}

func (c *compiler) visitCond(n condNode) {
        // TODO(jaq): split
        if n.cond != nil {
                n.cond.acceptVisitor(c)
        }
        c.emit(instr{op: jnm})
        // Save PC
        pc := len(c.prog) - 1
        for _, child := range n.children {
                child.acceptVisitor(c)
        }
        c.emit(instr{ret, 1})
        // rewrite jump label
        c.prog[pc].opnd = len(c.prog)
}

func (c *compiler) visitRegex(n regexNode) {
        re, err := regexp.Compile(n.pattern)
        if err != nil {
                log.Fatal("regex:", err)
        } else {
                c.re = append(c.re, re)
                c.emit(instr{match, len(c.re) - 1})
        }
}

func (c *compiler) visitString(n stringNode) {
        c.str = append(c.str, n.text)
        c.emit(instr{load, len(c.str) - 1})
}

func (c *compiler) visitId(n idNode) {
        i, ok := c.metrics[n.name]
        if !ok {
                m := &metric{name: n.name, typ: counter}
                metrics = append(metrics, m)
                c.emit(instr{push, len(metrics) - 1})
        } else {
                c.emit(instr{push, i})
        }
}

func (c *compiler) visitCapref(n caprefNode) {
        i, err := strconv.Atoi(n.name)
        if err != nil {
                log.Printf("capref: %s", err)
        }
        c.emit(instr{capref, i})
}

func (c *compiler) visitBuiltin(n builtinNode) {
        for _, child := range n.children {
                child.acceptVisitor(c)
        }
        c.emit(instr{op: builtin[n.name]})
}

func (v *vm) Start(lines chan string) {
        for {
                select {
                case line := <-lines:
                        v.Run(line)
                }
        }
}
