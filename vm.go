// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"fmt"
	"log"
	"regexp"
	"strconv"
	"time"
)

type opcode int

const (
	match     opcode = iota // Match a regular expression against input
	jnm                     // Jump if no match
	inc                     // Increment a variable value
	strptime                // Parse into the timestamp register
	timestamp               // Return value of timestamp register
	ret                     // Return, end program successfully
	push                    // Push operand onto stack
	capref                  // Push capture group reference at operand onto stack
	str                     // Push string constant at operand onto stack
	set                     // Set a variable value
	add                     // Add top values on stack and push to stack
	sub                     // Subtract tpo value from second top value on stack, and push to stack.
	mload                   // Load metric at operand onto top of stack.
	dload                   // Pop operand keys and metric off stack and load datum at metric[key] onto stack.
)

var opNames = map[opcode]string{
	match:     "match",
	jnm:       "jnm",
	inc:       "inc",
	strptime:  "strptime",
	timestamp: "timestamp",
	ret:       "ret",
	push:      "push",
	capref:    "capref",
	str:       "str",
	set:       "set",
	add:       "add",
	sub:       "sub",
	mload:     "mload",
	dload:     "dload",
}

var builtin = map[string]opcode{
	"strptime":  strptime,
	"timestamp": timestamp,
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
	matches map[int][]string
	time    time.Time
	stack   []interface{}
}

type vm struct {
	name string
	prog []instr

	// const regexps
	re []*regexp.Regexp
	// const strings
	str []string

	// data segment
	symtab *scope

	t thread
}

func (t *thread) Push(value interface{}) {
	t.stack = append(t.stack, value)
}

func (t *thread) Pop() (value interface{}) {
	last := len(t.stack) - 1
	value = t.stack[last]
	t.stack = t.stack[:last]
	return
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
		// match regex and store success
		// Store the results in the operandth element of the stack,
		// where i.opnd == the matched re index
		t.matches[i.opnd] = v.re[i.opnd].FindStringSubmatch(input)
		if t.matches[i.opnd] != nil {
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
		var delta int64 = 1
		// If opnd is nonzero, the delta is on the stack.
		if i.opnd > 0 {
			val := t.Pop()
			// Don't know what type it is on the stack though.
			switch n := val.(type) {
			case int:
				delta = int64(n)
			case int64:
				delta = n
			case string:
				var err error
				delta, err = strconv.ParseInt(n, 10, 64)
				if err != nil {
					return v.errorf("conversion of %q to numeric failed: %s", val, err)
				}
			default:
				return v.errorf("Unexpected type %T %q", val, val)
			}
		}
		switch d := t.Pop().(type) {
		case Incrementable:
			d.IncBy(delta, t.time)
		case int:
			m := metrics[d]
			m.IncBy(delta, t.time)
		default:
			return v.errorf("Unexpected type to increment: %T %q", d, d)
		}

	case set:
		// Set a gauge
		var value int64
		val := t.Pop()
		// Don't know what type it is on the stack though.
		switch n := val.(type) {
		case int:
			value = int64(n)
		case int64:
			value = n
		case string:
			var err error
			value, err = strconv.ParseInt(n, 10, 64)
			if err != nil {
				return v.errorf("conversion of %q to numeric failed: %s", val, err)
			}
		case time.Time:
			value = n.Unix()
		default:
			return v.errorf("Unexpected type %T %q\n", val, val)
		}
		switch d := t.Pop().(type) {
		case Settable:
			d.Set(value, t.time)
		case int:
			m := metrics[d]
			m.Set(value, t.time)
		default:
			return v.errorf("Unexpected type to set: %T %q", d, d)
		}

	case strptime:
		layout := t.Pop().(string)

		var ts string
		switch s := t.Pop().(type) {
		case string:
			ts = s

		case int: /* capref */
			// First find the match storage index on the stack
			re := t.Pop().(int)
			// Store the result from the re'th index at the s'th index
			ts = t.matches[re][s]
		}

		tm, err := time.Parse(layout, ts)
		if err != nil {
			return v.errorf("time.Parse(%s, %s) failed: %s", layout, ts, err)
		}
		t.time = tm

	case timestamp:
		t.Push(t.time)

	case capref:
		// Find the match storage index on the stack,
		re := t.Pop().(int)
		// Push the result from the re'th match at operandth index
		t.Push(t.matches[re][i.opnd])

	case str:
		t.Push(v.str[i.opnd])

	case ret:
		return true

	case push:
		t.Push(i.opnd)

	case add:
		var a, b int64
		switch d := t.Pop().(type) {
		case *Datum:
			a = d.Value
		case int64:
			a = d
		case int:
			a = int64(d)
		case time.Time:
			a = d.Unix()
		default:
			return v.errorf("Unexpected type for add %T %q\n", d, d)
		}
		switch d := t.Pop().(type) {
		case *Datum:
			b = d.Value
		case int64:
			b = d
		case int:
			b = int64(d)
		case time.Time:
			b = d.Unix()
		default:
			return v.errorf("Unexpected type for add %T %q\n", d, d)
		}
		t.Push(a + b)

	case sub:
		var a, b int64
		switch d := t.Pop().(type) {
		case *Datum:
			a = d.Value
		case int64:
			a = d
		case int:
			a = int64(d)
		case time.Time:
			a = d.Unix()
		default:
			return v.errorf("Unexpected type for sub %T %q\n", d, d)
		}
		switch d := t.Pop().(type) {
		case *Datum:
			b = d.Value
		case int64:
			b = d
		case int:
			b = int64(d)
		case time.Time:
			b = d.Unix()
		default:
			return v.errorf("Unexpected type for sub %T %q\n", d, d)
		}
		t.Push(b - a)

	case mload:
		t.Push(metrics[i.opnd])

	case dload:
		m := t.Pop().(*Metric)
		var keys []string
		for a := 0; a < i.opnd; a++ {
			keys = append(keys, t.Pop().(string))
		}
		h := key_hash(keys)
		if _, ok := m.Values[h]; !ok {
			m.Values[h] = &Datum{}
		}
		t.Push(m.Values[h])

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
	t.stack = make([]interface{}, 0)
	t.matches = make(map[int][]string, 0)
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
