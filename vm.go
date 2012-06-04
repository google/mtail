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
	match    opcode = iota // Match a regular expression against input
	jnm                    // Jump if no match
	inc                    // Increment an exported variable value
	strptime               // Parse into the timestamp register
	tag                    // Set a variable tag
	ret                    // Return, end program successfully
	push                   // Push operand onto stack
	capref                 // Push capture group reference at operand onto stack
	str                    // Push string constant at operand onto stack
	set                    // Set an exported variable value
	add                    // Add top values on stack and push to stack
	sub                    // Subtract tpo value from second top value on stack, and push to stack.
	stor                   // Store top of stack at location operand
	load                   // Load location at operand onto top of stack
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
	str:      "str",
	set:      "set",
	add:      "add",
	sub:      "sub",
	stor:     "stor",
	load:     "load",
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
	//tags    map[string]string
}

type vm struct {
	prog []instr

	// const regexps
	re []*regexp.Regexp
	// const strings
	str []string

	// data segment
	data map[int]interface{}

	stack Stack
	t     thread
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
		//t.tags = map[string]string{}
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
			val := v.stack.Pop()
			// Don't know what type it is on the stack though.
			switch n := val.(type) {
			case int:
				delta = n
			case string:
				var err error
				delta, err = strconv.Atoi(n)
				if err != nil {
					return v.errorf("conversion of %q to numeric failed: %s", val, err)
				}
			}
		}
		switch val := v.stack.Pop().(type) {
		case int:
			metrics[val].Inc(int64(delta), t.time)
		case string:
			/* look up the metric name */
			for _, metric := range metrics {
				if metric.Name == val {
					metric.Inc(int64(delta), t.time)
				}
			}
		}

	case set:
		// Set a gauge
		var value int
		val := v.stack.Pop()
		// Don't know what type it is on the stack though.
		switch n := val.(type) {
		case int:
			value = n
		case string:
			var err error
			value, err = strconv.Atoi(n)
			if err != nil {
				return v.errorf("conversion of %q to numeric failed: %s", val, err)
			}
		}
		m := v.stack.Pop().(int)
		metrics[m].Set(int64(value), t.time)
	case strptime:
		layout := v.stack.Pop().(string)

		var ts string
		switch s := v.stack.Pop().(type) {
		case string:
			ts = s

		case int:
			/* capref */
			ts = t.matches[s]
		}

		tm, err := time.Parse(layout, ts)
		if err != nil {
			return v.errorf("time.Parse(%s, %s) failed: %s", layout, ts, err)
		}
		t.time = tm
	case tag:
		v.stack.Pop()
		//k := v.stack.Pop()
		// //var key string
		// switch k.(type) {
		// case int:
		// 	key = strconv.Itoa(k.(int))
		// case string:
		// 	key = k.(string)
		// }
		// val := v.stack.Pop()
		// var value string
		// switch val.(type) {
		// case int:
		// 	value = strconv.Itoa(val.(int))
		// case string:
		// 	value = val.(string)
		// }
		// //t.tags[key] = value
	case capref:
		v.stack.Push(t.matches[i.opnd])
	case str:
		v.stack.Push(v.str[i.opnd])
	case ret:
		return true
	case push:
		v.stack.Push(i.opnd)
	case load:
		addr := v.stack.Pop().(int)
		v.stack.Push(v.data[addr])
	case stor:
		addr := v.stack.Pop().(int)
		value := v.stack.Pop()
		v.data[addr] = value
	case add:
		a := v.stack.Pop().(int)
		b := v.stack.Pop().(int)
		v.stack.Push(a + b)
	case sub:
		a := v.stack.Pop().(int)
		b := v.stack.Pop().(int)
		v.stack.Push(b - a)
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

	// Symbol table
	metric_indexes map[string]int // Cache of indexes for metric names already created.
}

func Compile(name string, input io.Reader) (*vm, []string) {
	p := NewParser(name, input)
	r := EmtailParse(p)
	if r != 0 || p == nil || len(p.errors) > 0 {
		return nil, p.errors
	}
	if *compile_only {
		output := unparse(p.root)
		log.Printf("Unparsing %s:\n%s", name, output)
	}
	file := filepath.Base(name)
	c := &compiler{name: file}
	c.metric_indexes = make(map[string]int, 0)
	c.compile(p.root)
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

func (c *compiler) compile(n node) {
	switch v := n.(type) {
	case *stmtlistNode:
		for _, child := range v.children {
			c.compile(child)
		}

	case *exprlistNode:
		for _, child := range v.children {
			c.compile(child)
		}

	case *declNode:
		var m *Metric
		if v.exported_name != "" {
			m = &Metric{Name: v.exported_name, Type: v.kind}
		} else {
			m = &Metric{Name: v.name, Type: v.kind}
		}
		metrics = append(metrics, m)
		c.metric_indexes[v.name] = len(metrics) - 1

	case *condNode:
		// TODO(jaq): split off a new goroutine thread instead of doing conds serially.
		if v.cond != nil {
			c.compile(v.cond)
		}
		c.emit(instr{op: jnm})
		// Save PC of jump instruction
		pc := len(c.prog) - 1
		for _, child := range v.children {
			c.compile(child)
		}
		c.emit(instr{ret, 1})
		// rewrite jump target
		c.prog[pc].opnd = len(c.prog)

	case *regexNode:
		re, err := regexp.Compile(v.pattern)
		if err != nil {
			c.errorf("%s", err)
			return
		} else {
			c.re = append(c.re, re)
			c.emit(instr{match, len(c.re) - 1})
		}

	case *stringNode:
		c.str = append(c.str, v.text)
		c.emit(instr{str, len(c.str) - 1})

	case *idNode:
		i, ok := c.metric_indexes[v.name]
		if !ok {
			c.errorf("undefined metric %s", v.name)
		}
		c.emit(instr{push, i})

	case *caprefNode:
		c.emit(instr{capref, v.index})

	case *builtinNode:
		c.compile(v.args)
		c.emit(instr{builtin[v.name], len(v.args.(*exprlistNode).children)})

	case *additiveExprNode:
		c.compile(v.lhs)
		c.compile(v.rhs)
		switch v.op {
		case '+':
			c.emit(instr{op: add})
		case '-':
			c.emit(instr{op: sub})
		}

	case *assignExprNode:
		c.compile(v.lhs)
		c.compile(v.rhs)
		c.emit(instr{op: stor})

	case *indexedExprNode:
		c.compile(v.lhs)
		c.compile(v.index)
		c.emit(instr{op: load})

	default:
		c.errorf("undefined node type %T", n)
	}
}
