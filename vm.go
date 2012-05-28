// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"path/filepath"
	"regexp"
	"strconv"
	"time"
)

var compile_only *bool = flag.Bool("compile_only", false, "Compile programs only.")

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
	str
	set
	add
	sub
	stor
	load
)

var opNames = map[opcode]string{
	match:    "match",    // Match a regular expression against input
	jnm:      "jnm",      // Jump if no match
	inc:      "inc",      // Increment an exported variable value
	strptime: "strptime", // Parse into the timestamp register
	tag:      "tag",      // Set a variable tag
	ret:      "ret",      // Return, end program successfully
	push:     "push",     // Push operand on to stack
	capref:   "capref",   // Push capref at operand onto stack
	str:      "str",      // Push string at operand onto stack
	set:      "set",      // Set an exported variable value
	add:      "add",      // Add top values on stack and push to stack
	sub:      "sub",      // Subtract tpo value from second top value on stack,  and push to stack.
	stor:     "stor",     // Store top of stack at location operand
	load:     "load",     // Load location at operand onto top of stack
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
		layout := v.pop().(string)
		s := v.pop().(string)
		tm, err := time.Parse(layout, s)
		if err != nil {
			return v.errorf("time.Parse(%s, %s) failed: %s", layout, s, err)
		}
		t.time = tm
	case tag:
		k := v.pop()
		var key string
		switch k.(type) {
		case int:
			key = strconv.Itoa(k.(int))
		case string:
			key = k.(string)
		}
		val := v.pop()
		var value string
		switch val.(type) {
		case int:
			value = strconv.Itoa(val.(int))
		case string:
			value = val.(string)
		}
		t.tags[key] = value
	case capref:
		v.push(t.matches[i.opnd])
	case str:
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
	r := EmtailParse(p)
	if r != 0 || p == nil || len(p.errors) > 0 {
		return nil, p.errors
	}
	if *compile_only {
		u := &unparser{}
		p.root.acceptVisitor(u)
		log.Printf("Unparsing %s:\n%s", name, u.output)
	}
	file := filepath.Base(name)
	c := &compiler{name: file}
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
	// TODO(jaq): split off a new goroutine thread instead of doing conds serially.
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
	c.emit(instr{str, len(c.str) - 1})
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
			m := &Metric{Name: n.name, Type: typ[c.builtin], Tags: map[string]string{"prog": c.name}}
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
	case "tag", "strptime":
		// Turn IDs into strings.
		c.str = append(c.str, n.name)
		c.emit(instr{push, len(c.str) - 1})
	}
}

func (c *compiler) visitCapref(n caprefNode) {
	c.emit(instr{capref, n.index})
}

func (c *compiler) visitBuiltin(n builtinNode) {
	c.builtin = n.name
	n.args.acceptVisitor(c)
	c.emit(instr{builtin[n.name], len(n.args.children)})
	c.builtin = ""
}

func (c *compiler) visitAdditiveExpr(a additiveExprNode) {
	a.lhs.acceptVisitor(c)
	a.rhs.acceptVisitor(c)
	switch a.op {
	case '+':
		c.emit(instr{op: add})
	case '-':
		c.emit(instr{op: sub})
	}
}

func (c *compiler) visitAssignExpr(a assignExprNode) {
	a.lhs.acceptVisitor(c)
	a.rhs.acceptVisitor(c)
	c.emit(instr{op: stor})
}

func (c *compiler) visitIndexedExpr(i indexedExprNode) {
	i.lhs.acceptVisitor(c)
	i.index.acceptVisitor(c)
	c.emit(instr{op: load})
}
