// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package vm provides a compiler and virtual machine environment for executing
// mtail programs.
package vm

import (
	"fmt"
	"math"
	"os"
	"regexp"
	"runtime/debug"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/golang/glog"

	"github.com/google/mtail/metrics"
)

type opcode int

const (
	match     opcode = iota // Match a regular expression against input, and set the match register.
	cmp                     // Compare two values on the stack and set the match register.
	jnm                     // Jump if no match.
	jm                      // Jump if match.
	inc                     // Increment a variable value
	strptime                // Parse into the timestamp register
	timestamp               // Return value of timestamp register onto TOS.
	settime                 // Set timestamp register to value at TOS.
	push                    // Push operand onto stack
	capref                  // Push capture group reference at operand onto stack
	str                     // Push string constant at operand onto stack
	set                     // Set a variable value
	add                     // Add top values on stack and push to stack
	sub                     // Subtract top value from second top value on stack, and push to stack.
	mul                     // Multiply top values on stack and push to stack
	div                     // Divide top value into second top on stack, and push
	pow                     // Put second TOS to power of TOS, and push.
	and                     // Bitwise AND the 2 at top of stack, and push result
	or                      // Bitwise OR the 2 at top of stack, and push result
	xor                     // Bitwise XOR the 2 at top of stack, and push result
	not                     // Bitwise NOT the top of stack, and push result
	shl                     // Shift TOS left, push result
	shr                     // Shift TOS right, push result
	mload                   // Load metric at operand onto top of stack
	dload                   // Pop operand keys and metric off stack and load datum at metric[key] onto stack.
	tolower                 // Convert the string at the top of the stack to lowercase.
	length                  // Compute the length of a string.
	strtol                  // Convert a string to a number, given a base.
)

var opNames = map[opcode]string{
	match:     "match",
	cmp:       "cmp",
	jnm:       "jnm",
	jm:        "jm",
	inc:       "inc",
	strptime:  "strptime",
	timestamp: "timestamp",
	settime:   "settime",
	push:      "push",
	capref:    "capref",
	str:       "str",
	set:       "set",
	add:       "add",
	sub:       "sub",
	mul:       "mul",
	div:       "div",
	pow:       "pow",
	shl:       "shl",
	shr:       "shr",
	and:       "and",
	or:        "or",
	xor:       "xor",
	not:       "not",
	mload:     "mload",
	dload:     "dload",
	tolower:   "tolower",
	length:    "length",
	strtol:    "strtol",
}

var builtin = map[string]opcode{
	"timestamp": timestamp,
	"len":       length,
	"settime":   settime,
	"strptime":  strptime,
	"strtol":    strtol,
	"tolower":   tolower,
}

type instr struct {
	op   opcode
	opnd interface{}
}

type thread struct {
	pc      int              // Program counter.
	match   bool             // Match register.
	matches map[int][]string // Match result variables.
	time    time.Time        // Time register.
	stack   []interface{}    // Data stack.
}

// VM describes the virtual machine for each program.  It contains virtual
// segments of the executable bytecode, constant data (string and regular
// expressions), mutable state (metrics), and a stack for the current thread of
// execution.
type VM struct {
	name string
	prog []instr

	re  []*regexp.Regexp  // Regular expression constants
	str []string          // String constants
	m   []*metrics.Metric // Metrics accessible to this program.

	timeMemos map[string]time.Time // memo of time string parse results

	t *thread // Current thread of execution

	input string // Log line input to this round of execution.

	terminate bool // Flag to stop the VM program.

	syslogUseCurrentYear bool // Overwrite zero years with the current year in a strptime.
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
func (v *VM) errorf(format string, args ...interface{}) {
	glog.Infof("Runtime error: "+format+"\n", args...)
	glog.Infof("VM stack:\n%s", debug.Stack())
	glog.Infof("Dumping vm state")
	glog.Infof("Regexes:")
	for i, re := range v.re {
		glog.Infof("\t%4d %v", i, re)
	}
	glog.Infof("Strings:")
	for i, s := range v.str {
		glog.Infof("\t%4d %q", i, s)
	}
	glog.Infof("Thread:")
	glog.Infof("\tPC %v", v.t.pc)
	glog.Infof("\tMatch %v", v.t.match)
	glog.Infof("\tMatches %v", v.t.matches)
	glog.Infof("\tTimestamp %v", v.t.time)
	glog.Infof("\tStack %v", v.t.stack)
	glog.Infof("Program:")
	for i, instr := range v.prog {
		glog.Infof("\t%4d %8s %d", i, opNames[instr.op], instr.opnd)
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
	case *metrics.Datum:
		return n.Value, nil
	}
	return 0, fmt.Errorf("unexpected numeric type %T %q", val, val)
}

// Execute performs an instruction cycle in the VM -- acting on the current
// instruction, and returns a boolean indicating if the current thread should
// terminate.
func (v *VM) execute(t *thread, i instr) {
	switch i.op {
	case match:
		// match regex and store success
		// Store the results in the operandth element of the stack,
		// where i.opnd == the matched re index
		index := i.opnd.(int)
		t.matches[index] = v.re[index].FindStringSubmatch(v.input)
		t.match = t.matches[index] != nil

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
			t.pc = i.opnd.(int)
		}

	case jm:
		if t.match {
			t.pc = i.opnd.(int)
		}

	case inc:
		// increment a counter
		var delta int64 = 1
		// If opnd is non-nil, the delta is on the stack.
		if i.opnd != nil {
			var err error
			delta, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		switch n := t.Pop().(type) {
		case metrics.Incrementable:
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
		case metrics.Settable:
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
		if tm, ok := v.timeMemos[ts]; !ok {
			tm, err := time.Parse(layout, ts)
			if err != nil {
				v.errorf("time.Parse(%s, %s) failed: %s", layout, ts, err)
			}
			// Hack for yearless syslog.
			if tm.Year() == 0 && v.syslogUseCurrentYear {
				// No .UTC() as we use local time to match the local log.
				tm = tm.AddDate(time.Now().Year(), 0, 0)
			}
			v.timeMemos[ts] = tm
			t.time = tm
		} else {
			t.time = tm
		}

	case timestamp:
		// Put the time register onto the stack
		t.Push(t.time.Unix())

	case settime:
		// Pop TOS and store in time register
		t.time = time.Unix(t.Pop().(int64), 0).UTC()

	case capref:
		// Put a capture group reference onto the stack.
		// First find the match storage index on the stack,
		re := t.Pop().(int)
		// Push the result from the re'th match at operandth index
		t.Push(t.matches[re][i.opnd.(int)])

	case str:
		// Put a string constant onto the stack
		t.Push(v.str[i.opnd.(int)])

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

	case mul:
		// Multiply two values at TOS, push result
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a * b)

	case div:
		// Divide two values at TOS, push result
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a / b)

	case pow:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(int64(math.Pow(float64(a), float64(b))))

	case shl:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a << uint(b))

	case shr:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a >> uint(b))

	case and:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a & b)

	case or:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a | b)

	case xor:
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(a ^ b)

	case not:
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(^a)

	case mload:
		// Load a metric at operand onto stack
		t.Push(v.m[i.opnd.(int)])

	case dload:
		// Load a datum from metric at TOS onto stack
		//fmt.Printf("Stack: %v\n", t.stack)
		m := t.Pop().(*metrics.Metric)
		//fmt.Printf("Metric: %v\n", m)
		index := i.opnd.(int)
		keys := make([]string, index)
		//fmt.Printf("keys: %v\n", keys)
		for a := 0; a < index; a++ {
			s := t.Pop().(string)
			//fmt.Printf("s: %v\n", s)
			keys[a] = s
			//fmt.Printf("Keys: %v\n", keys)
		}
		//fmt.Printf("Keys: %v\n", keys)
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

	case strtol:
		base, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		str := t.Pop().(string)
		i, err := strconv.ParseInt(str, int(base), 64)
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(i)

	default:
		v.errorf("illegal instruction: %q", i.op)
	}
}

// processLine handles the incoming lines from the input channel, by running a
// fetch-execute cycle on the VM bytecode with the line as input to the
// program, until termination.
func (v *VM) processLine(input string) {
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
}

// Run executes the virtual machine on each line of input received.  When the
// input closes, it signals to the loader that it has terminated by closing the
// shutdown channel.
func (v *VM) Run(lines <-chan string, shutdown chan<- struct{}) {
	glog.Infof("Starting program %s", v.name)
	defer close(shutdown)
	for line := range lines {
		v.processLine(line)
	}
	glog.Infof("Stopping program %s", v.name)
}

// New creates a new virtual machine with the given name, and compiler
// artifacts for executable and data segments.
func New(name string, re []*regexp.Regexp, str []string, m []*metrics.Metric, prog []instr, syslogUseCurrentYear bool) *VM {
	return &VM{
		name:                 name,
		re:                   re,
		str:                  str,
		m:                    m,
		prog:                 prog,
		timeMemos:            make(map[string]time.Time, 0),
		syslogUseCurrentYear: syslogUseCurrentYear,
	}
}

// DumpByteCode emits the program disassembly and data to standard out.
func (v *VM) DumpByteCode(name string) {
	fmt.Printf("Prog %s\n", name)
	fmt.Println("Metrics")
	for i, m := range v.m {
		if m.Program == v.name {
			fmt.Printf(" %8d %s\n", i, m)
		}
	}
	fmt.Println("REs")
	for i, re := range v.re {
		fmt.Printf(" %8d /%s/\n", i, re)
	}
	fmt.Println("Strings")
	for i, str := range v.str {
		fmt.Printf(" %8d \"%s\"\n", i, str)
	}
	w := new(tabwriter.Writer)
	w.Init(os.Stdout, 0, 0, 1, ' ', tabwriter.AlignRight)

	fmt.Fprintln(w, "disasm\tl\top\topnd\t")
	for n, i := range v.prog {
		fmt.Fprintf(w, "\t%d\t%s\t%d\t\n", n, opNames[i.op], i.opnd)
	}
	w.Flush()
}
