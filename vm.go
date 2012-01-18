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
	match opcode = iota
	jnm
	inc
	strptime
	tag
	ret
	push
	capref
	load
	set
)

var opNames = map[opcode]string{
	match:    "match",
	jnm:      "jnm",
	inc:      "inc",
	strptime: "strptime",
	tag:      "tag",
	ret:      "ret",
	push:     "push",
	capref:   "capref",
	load:     "load",
	set:      "set",
}

var builtin = map[string]opcode{
	"inc":      inc,
	"set":      set,
	"strptime": strptime,
	"tag":      tag,
}

type instr struct {
	op   opcode
	opnd int
}

func (i instr) String() string {
	return fmt.Sprintf("%s %d", opNames[i.op], i.opnd)
}

type thread struct {
	pc      int
	reg     int
	matches []string
	time    time.Time
	tags    map[string]string
}

type vm struct {
	prog []instr
	re   []*regexp.Regexp

	str []string

	stack []interface{}
	t     thread
}

func (v *vm) pop() interface{} {
	m := v.stack[len(v.stack)-1]
	v.stack = v.stack[:len(v.stack)-1]
	return m
}

func (v *vm) push(opnd interface{}) {
	v.stack = append(v.stack, opnd)
}

func (v *vm) errorf(format string, args ...interface{}) bool {
	log.Printf("Runtime error: "+format+"\n", args...)
	v.t.reg = 0
	return true
}

// Execute acts on the current instruction, and returns a boolean indicating
// if the current thread should terminate.
func (v *vm) execute(t *thread, i instr, input string) bool {
	switch i.op {
	case match:
		t.tags = map[string]string{}
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
			return false
		}
	case inc:
		// increment a counter
		delta := 1
		// If there's more than one arg, increment by delta.
		if i.opnd > 1 {
			val := v.pop()
			// Don't know what type it is on the stack though.
			switch val.(type) {
			case int:
				delta = val.(int)
			case string:
				var err error
				delta, err = strconv.Atoi(val.(string))
				if err != nil {
					return v.errorf("conversion to numeric failed: %s", err)
				}
			}
		}
		m := v.pop().(int)
		metric_lock.Lock()
		defer metric_lock.Unlock()
		metrics[m].Value += int64(delta)
		if t.time.IsZero() {
			metrics[m].Time = time.Now()
		} else {
			metrics[m].Time = t.time
		}
	case set:
		// Set a gauge
		var value int
		val := v.pop()
		// Don't know what type it is on the stack though.
		switch val.(type) {
		case int:
			value = val.(int)
		case string:
			var err error
			value, err = strconv.Atoi(val.(string))
			if err != nil {
				return v.errorf("conversion to numeric failed: %s", err)
			}
		}
		m := v.pop().(int)
		metric_lock.Lock()
		defer metric_lock.Unlock()
		metrics[m].Value = int64(value)
		if t.time.IsZero() {
			metrics[m].Time = time.Now()
		} else {
			metrics[m].Time = t.time
		}
	case strptime:
		s := v.pop().(string)
		layout := v.pop().(string)
		tm, err := time.Parse(layout, s)
		if err != nil {
			return v.errorf("time parse failed: %s", err)
		}
		t.time = tm
	case tag:
		val := v.pop().(string)
		key := v.pop().(string)
		t.tags[key] = val
	case capref:
		v.push(t.matches[i.opnd])
	case load:
		v.push(v.str[i.opnd])
	case ret:
		return true
	case push:
		v.push(i.opnd)
	default:
		return v.errorf("illegal instruction: %q", i.op)
	}
	t.pc++
	return false
}

// Run fetches and executes each instruction in the program on the input string
// until termination. It returns a boolean indicating a successful match.
func (v *vm) Run(input string) bool {
	t := v.t
	for {
		if t.pc >= len(v.prog) {
			return false
		}
		i := v.prog[t.pc]

		terminate := v.execute(&t, i, input)
		if terminate {
			// t.reg contains the result of the last match.
			return t.reg == 1
		}
	}
	panic("not reached")
}

type compiler struct {
	name   string   // Name of the program.
	errors []string // Compile errors.

	prog []instr          // The emitted program.
	str  []string         // Static strings.
	re   []*regexp.Regexp // Static regular expressions.

	metrics map[string]int // Cache of indexes for metric names already created.
	refs    []int          // Capture group references.
	builtin string         // Name of a builtin being visited.
}

func Compile(name string, input io.Reader) (*vm, []string) {
	p := NewParser(name, input)
	EmtailParse(p)
	if p == nil || len(p.errors) > 0 {
		return nil, p.errors
	}
	if *compile_only {
		u := &unparser{}
		p.root.acceptVisitor(u)
		log.Printf("Unparsing %s:\n%s", name, u.output)
	}
	c := &compiler{name: name}
	p.root.acceptVisitor(c)
	if len(c.errors) > 0 {
		return nil, c.errors
	}
	vm := &vm{
		re:   c.re,
		str:  c.str,
		prog: c.prog}
	return vm, nil
}

func (c *compiler) errorf(format string, args ...interface{}) {
	e := fmt.Sprintf(c.name+": "+format, args...)
	c.errors = append(c.errors, e)
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
	// Save PC of jump instruction
	pc := len(c.prog) - 1
	for _, child := range n.children {
		child.acceptVisitor(c)
	}
	c.emit(instr{ret, 1})
	// rewrite jump target
	c.prog[pc].opnd = len(c.prog)
}

func (c *compiler) visitRegex(n regexNode) {
	re, err := regexp.Compile(n.pattern)
	if err != nil {
		c.errorf("%s", err)
		return
	} else {
		c.re = append(c.re, re)
		c.emit(instr{match, len(c.re) - 1})
	}
}

func (c *compiler) visitString(n stringNode) {
	c.str = append(c.str, n.text)
	c.emit(instr{load, len(c.str) - 1})
}

var typ = map[string]mtype{
	"inc": Counter,
	"set": Gauge,
}

func (c *compiler) visitId(n idNode) {
	switch c.builtin {
	case "":
	case "inc", "set":
		i, ok := c.metrics[n.name]
		if !ok {
			m := &Metric{Name: n.name, Type: typ[c.builtin]}
			metrics = append(metrics, m)
			c.emit(instr{push, len(metrics) - 1})
		} else {
			// Check that the metric has the correct type.
			if metrics[i].Type != typ[c.builtin] {
				c.errorf("invalid metric type for %s: %s is a %s, expected %s", c.builtin, metrics[i].Name, metrics[i].Type, typ[c.builtin])
				return
			}
			c.emit(instr{push, i})
		}
	}
}

func (c *compiler) visitCapref(n caprefNode) {
	i, err := strconv.Atoi(n.name)
	if err != nil {
		c.errorf("%s", err)
		//log.Printf("capref: %s", err)
		return
	}
	c.emit(instr{capref, i})
}

func (c *compiler) visitBuiltin(n builtinNode) {
	c.builtin = n.name
	n.args.acceptVisitor(c)
	c.emit(instr{builtin[n.name], len(n.args.children)})
	c.builtin = ""
}

func (v *vm) Start(lines chan string) {
	for {
		select {
		case line := <-lines:
			v.Run(line)
		}
	}
}
