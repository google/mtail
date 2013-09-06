// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"expvar"
	"flag"
	"fmt"
	"log"
	"regexp"
	"runtime/debug"
	"sort"
	"strconv"
	"strings"
	"time"
)

var (
	syslog_use_current_year = flag.Bool("syslog_use_current_year", true, "Patch yearless timestamps with the present year.")
)

var (
	line_count = expvar.NewInt("line_count")
)

type opcode int

const (
	match     opcode = iota // Match a regular expression against input, and set the match register.
	cmp                     // Compare two values on the stack and set the match register.
	jnm                     // Jump if no match.
	jm                      // Jump if match.
	inc                     // Increment a variable value
	strptime                // Parse into the timestamp register
	timestamp               // Return value of timestamp register
	push                    // Push operand onto stack
	capref                  // Push capture group reference at operand onto stack
	str                     // Push string constant at operand onto stack
	set                     // Set a variable value
	add                     // Add top values on stack and push to stack
	sub                     // Subtract tpo value from second top value on stack, and push to stack.
	mload                   // Load metric at operand onto top of stack.
	dload                   // Pop operand keys and metric off stack and load datum at metric[key] onto stack.
	tolower                 // Convert the string at the top of the stack to lowercase.
	length                  // Compute the length of a string.
)

var opNames = map[opcode]string{
	match:     "match",
	cmp:       "cmp",
	jnm:       "jnm",
	jm:        "jm",
	inc:       "inc",
	strptime:  "strptime",
	timestamp: "timestamp",
	push:      "push",
	capref:    "capref",
	str:       "str",
	set:       "set",
	add:       "add",
	sub:       "sub",
	mload:     "mload",
	dload:     "dload",
	tolower:   "tolower",
	length:    "length",
}

var builtin = map[string]opcode{
	"strptime":  strptime,
	"timestamp": timestamp,
	"tolower":   tolower,
	"len":       length,
}

type instr struct {
	op   opcode
	opnd int
}

type thread struct {
	pc      int              // Program counter.
	match   bool             // Match register.
	matches map[int][]string // Match result variables.
	time    time.Time        // Time register.
	stack   []interface{}    // Data stack.
}

type vm struct {
	name string
	prog []instr

	re  []*regexp.Regexp // Regular expression constants
	str []string         // String constants
	m   []*Metric        // Metrics accessible to this program.

	ts_mem map[string]time.Time // memo of time string parse results

	t *thread // Current thread of execution

	input string // Log line input to this round of execution.

	terminate bool // Flag to stop the VM program.
}

// Push a value onto the stack
func (t *thread) Push(value interface{}) {
	t.stack = append(t.stack, value)
}

// Pop a value off the stack
func (t *thread) Pop() (value interface{}) {
	last := len(t.stack) - 1
	value = t.stack[last]
	t.stack = t.stack[:last]
	return
}

// Log a runtime error and terminate the program
func (v *vm) errorf(format string, args ...interface{}) {
	log.Printf("Runtime error: "+format+"\n", args...)
	log.Printf("VM stack:\n%s", debug.Stack())
	log.Printf("Dumping vm state\n")
	log.Printf("Regexes:\n")
	for i, re := range v.re {
		log.Printf("\t%4d %v\n", i, re)
	}
	log.Printf("Strings:\n")
	for i, s := range v.str {
		log.Printf("\t%4d %q\n", i, s)
	}
	log.Printf("Thread:\n")
	log.Printf("\tPC %v\n", v.t.pc)
	log.Printf("\tMatch %v\n", v.t.match)
	log.Printf("\tMatches %v\n", v.t.matches)
	log.Printf("\tTimestamp %v\n", v.t.time)
	log.Printf("\tStack %v\n", v.t.stack)
	log.Printf("Program:\n")
	for i, instr := range v.prog {
		log.Printf("\t%4d %8s %d\n", i, opNames[instr.op], instr.opnd)
	}
	v.terminate = true
}

func (t *thread) PopInt() (int64, error) {
	val := t.Pop()
	switch n := val.(type) {
	case int64:
		return n, nil
	case int:
		return int64(n), nil
	case string:
		r, err := strconv.ParseInt(n, 10, 64)
		if err != nil {
			return 0, fmt.Errorf("conversion of %q to numeric failed: %s", val, err)
		}
		return r, nil
	case time.Time:
		return n.Unix(), nil
	case *Datum:
		return n.Value, nil
	}
	return 0, fmt.Errorf("unexpected numeric type %T %q", val, val)
}

// Execute acts on the current instruction, and returns a boolean indicating
// if the current thread should terminate.
func (v *vm) execute(t *thread, i instr) {
	switch i.op {
	case match:
		// match regex and store success
		// Store the results in the operandth element of the stack,
		// where i.opnd == the matched re index
		t.matches[i.opnd] = v.re[i.opnd].FindStringSubmatch(v.input)
		t.match = t.matches[i.opnd] != nil

	case cmp:
		// Compare two elements on the stack.
		// Set the match register based on the truthiness of the comparison.
		// Operand contains the expected result.
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}

		switch i.opnd {
		case -1:
			t.match = a < b
		case 0:
			t.match = a == b
		case 1:
			t.match = a > b
		}

	case jnm:
		if !t.match {
			t.pc = i.opnd
		}

	case jm:
		if t.match {
			t.pc = i.opnd
		}

	case inc:
		// increment a counter
		var delta int64 = 1
		// If opnd is nonzero, the delta is on the stack.
		if i.opnd > 0 {
			var err error
			delta, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		switch n := t.Pop().(type) {
		case Incrementable:
			n.IncBy(delta, t.time)
		case int:
			m := v.m[n]
			d, err := m.GetDatum()
			if err != nil {
				v.errorf("GetDatum failed: %s", err)
			}
			d.IncBy(delta, t.time)
		default:
			v.errorf("Unexpected type to increment: %T %q", n, n)
		}

	case set:
		// Set a gauge
		value, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}

		switch n := t.Pop().(type) {
		case Settable:
			n.Set(value, t.time)
		case int:
			m := v.m[n]
			d, err := m.GetDatum()
			if err != nil {
				v.errorf("GetDatum failed: %s", err)
			}
			d.Set(value, t.time)
		default:
			v.errorf("Unexpected type to set: %T %q", n, n)
		}

	case strptime:
		// Parse a time string into the time register
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
		if tm, ok := v.ts_mem[ts]; !ok {
			tm, err := time.Parse(layout, ts)
			if err != nil {
				v.errorf("time.Parse(%s, %s) failed: %s", layout, ts, err)
			}
			// Hack for yearless syslog.
			if tm.Year() == 0 && *syslog_use_current_year {
				tm = tm.AddDate(time.Now().Year(), 0, 0)
			}
			v.ts_mem[ts] = tm
			t.time = tm
		} else {
			t.time = tm
		}

	case timestamp:
		// Put the time register onto the stack
		t.Push(t.time)

	case capref:
		// Put a capture group reference onto the stack.
		// First find the match storage index on the stack,
		re := t.Pop().(int)
		// Push the result from the re'th match at operandth index
		t.Push(t.matches[re][i.opnd])

	case str:
		// Put a string constant onto the stack
		t.Push(v.str[i.opnd])

	case push:
		// Push a value onto the stack
		t.Push(i.opnd)

	case add:
		// Add two values at TOS, and push result onto stack
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a + b)

	case sub:
		// Subtract two values at TOS, push result onto stack
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a - b)

	case mload:
		// Load a metric at operand onto stack
		t.Push(v.m[i.opnd])

	case dload:
		// Load a datum from metric at TOS onto stack
		//fmt.Printf("Stack: %v\n", t.stack)
		m := t.Pop().(*Metric)
		//fmt.Printf("Metric: %v\n", m)
		keys := make([]string, i.opnd)
		//fmt.Printf("keys: %v\n", keys)
		for a := 0; a < i.opnd; a++ {
			s := t.Pop().(string)
			//fmt.Printf("s: %v\n", s)
			keys[a] = s
			//fmt.Printf("Keys: %v\n", keys)
		}
		//fmt.Printf("Keys: %v\n", keys)
		sort.Sort(sort.StringSlice(keys))
		d, err := m.GetDatum(keys...)
		if err != nil {
			v.errorf("GetDatum failed: %s", err)
		}
		//fmt.Printf("Found %v\n", d)
		t.Push(d)

	case tolower:
		// Lowercase a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(strings.ToLower(s))

	case length:
		// Compute the length of a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(len(s))

	default:
		v.errorf("illegal instruction: %q", i.op)
	}
}

// Run fetches and executes each instruction in the program on the input string
// until termination. It returns a boolean indicating a successful action was
// taken.
func (v *vm) Run(input string) {
	t := new(thread)
	v.t = t
	v.input = input
	t.stack = make([]interface{}, 0)
	t.matches = make(map[int][]string, 0)
	for {
		if t.pc >= len(v.prog) {
			return
		}
		i := v.prog[t.pc]
		t.pc++
		v.execute(t, i)
		if v.terminate {
			return
		}
	}
	panic("not reached")
}

func newVm(name string, re []*regexp.Regexp, str []string, m []*Metric, prog []instr) *vm {
	return &vm{
		name:   name,
		re:     re,
		str:    str,
		m:      m,
		prog:   prog,
		ts_mem: make(map[string]time.Time, 0),
	}
}

// vms contains a list of virtual machines to execute when each new line is received
type engine map[string]*vm

func (e engine) addVm(name string, v *vm) {
	e[name] = v
}

func (e engine) removeVm(name string) {
	delete(e, name)
}

// RunVms receives a line from a channel and sends it to all VMs.
func (e *engine) run(lines chan string) {
	for {
		select {
		// TODO(jaq): stop?
		case line := <-lines:
			line_count.Add(1)
			for _, v := range *e {
				// TODO(jaq): Instead of forking a goroutine each time, set up a line channel to each VM.
				v.Run(line)
			}
		}
	}
}
