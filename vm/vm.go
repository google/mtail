// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package vm provides a compiler and virtual machine environment for executing
// mtail programs.
package vm

import (
	"bytes"
	"fmt"
	"math"
	"regexp"
	"runtime/debug"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/golang/glog"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

type opcode int

const (
	match      opcode = iota // Match a regular expression against input, and set the match register.
	cmp                      // Compare two values on the stack and set the match register.
	jnm                      // Jump if no match.
	jm                       // Jump if match.
	jmp                      // Unconditional jump
	inc                      // Increment a variable value
	strptime                 // Parse into the timestamp register
	timestamp                // Return value of timestamp register onto TOS.
	settime                  // Set timestamp register to value at TOS.
	push                     // Push operand onto stack
	capref                   // Push capture group reference at operand onto stack
	str                      // Push string constant at operand onto stack
	iset                     // Set a variable value
	iadd                     // Add top values on stack and push to stack
	isub                     // Subtract top value from second top value on stack, and push to stack.
	imul                     // Multiply top values on stack and push to stack
	idiv                     // Divide top value into second top on stack, and push
	imod                     // Integer divide top value into second top on stack, and push remainder
	ipow                     // Put second TOS to power of TOS, and push.
	and                      // Bitwise AND the 2 at top of stack, and push result
	or                       // Bitwise OR the 2 at top of stack, and push result
	xor                      // Bitwise XOR the 2 at top of stack, and push result
	not                      // Bitwise NOT the top of stack, and push result
	shl                      // Shift TOS left, push result
	shr                      // Shift TOS right, push result
	mload                    // Load metric at operand onto top of stack
	dload                    // Pop `operand` keys and metric off stack, and push datum at metric[key,...] onto stack.
	tolower                  // Convert the string at the top of the stack to lowercase.
	length                   // Compute the length of a string.
	strtol                   // Convert a string to a number, given a base.
	setmatched               // Set "matched" flag
	otherwise                // Only match if "matched" flag is false.
	del                      //  Pop `operand` keys and metric off stack, and remove the datum at metric[key,...] from memory

	// Floating point ops
	fadd
	fsub
	fmul
	fdiv
	fmod
	fpow
	fset // Floating point assignment
)

var opNames = map[opcode]string{
	match:      "match",
	cmp:        "cmp",
	jnm:        "jnm",
	jm:         "jm",
	jmp:        "jmp",
	inc:        "inc",
	strptime:   "strptime",
	timestamp:  "timestamp",
	settime:    "settime",
	push:       "push",
	capref:     "capref",
	str:        "str",
	iset:       "iset",
	iadd:       "iadd",
	isub:       "isub",
	imul:       "imul",
	idiv:       "idiv",
	imod:       "imod",
	ipow:       "ipow",
	shl:        "shl",
	shr:        "shr",
	and:        "and",
	or:         "or",
	xor:        "xor",
	not:        "not",
	mload:      "mload",
	dload:      "dload",
	tolower:    "tolower",
	length:     "length",
	strtol:     "strtol",
	setmatched: "setmatched",
	otherwise:  "otherwise",
	fadd:       "fadd",
	fsub:       "fsub",
	fmul:       "fmul",
	fdiv:       "fdiv",
	fmod:       "fmod",
	fpow:       "fpow",
	fset:       "fset",
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

// debug print for instructions
func (i instr) String() string {
	return fmt.Sprintf("{%s %v}", opNames[i.op], i.opnd)
}

type thread struct {
	pc      int              // Program counter.
	match   bool             // Match register.
	matched bool             // Flag set if any match has been found.
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
	glog.Infof(v.name+": Runtime error: "+format+"\n", args...)
	glog.Infof("VM stack:\n%s", debug.Stack())
	glog.Infof("Dumping vm state")
	glog.Infof("Name: %s", v.name)
	glog.Infof("Input: %q", v.input)
	glog.Infof("Thread:")
	glog.Infof(" PC %v", v.t.pc-1)
	glog.Infof(" Match %v", v.t.match)
	glog.Infof(" Matched %v", v.t.matched)
	glog.Infof(" Matches %v", v.t.matches)
	glog.Infof(" Timestamp %v", v.t.time)
	glog.Infof(" Stack %v", v.t.stack)
	glog.Infof(v.DumpByteCode(v.name))
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
			return 0, fmt.Errorf("conversion of %q to int failed: %s", val, err)
		}
		return r, nil
	case time.Time:
		return n.Unix(), nil
	case datum.Datum:
		return datum.GetInt(n), nil
	}
	return 0, fmt.Errorf("unexpected int type %T %q", val, val)
}

func (t *thread) PopFloat() (float64, error) {
	val := t.Pop()
	switch n := val.(type) {
	case float64:
		return n, nil
	case int:
		return float64(n), nil
	case string:
		r, err := strconv.ParseFloat(n, 64)
		if err != nil {
			return 0, fmt.Errorf("conversion of %q to float failed: %s", val, err)
		}
		return r, nil
	case datum.Datum:
		return datum.GetFloat(n), nil
	}
	return 0, fmt.Errorf("unexpected float type %T %q", val, val)
}

func compareInt(a, b int64, opnd int) (bool, error) {
	switch opnd {
	case -1:
		return a < b, nil
	case 0:
		return a == b, nil
	case 1:
		return a > b, nil
	default:
		return false, fmt.Errorf("unexpected operator type %q", opnd)
	}
}

func compareFloat(a, b float64, opnd int) (bool, error) {
	switch opnd {
	case -1:
		return a < b, nil
	case 0:
		return a == b, nil
	case 1:
		return a > b, nil
	default:
		return false, fmt.Errorf("unexpected operator type %q", opnd)
	}
}

func compareString(a, b string, opnd int) (bool, error) {
	switch opnd {
	case -1:
		return a < b, nil
	case 0:
		return a == b, nil
	case 1:
		return a > b, nil
	default:
		return false, fmt.Errorf("unexpected operator type %q", opnd)
	}
}

func compare(a, b interface{}, opnd int) (bool, error) {
	lxF, lxIsFloat := a.(float64)
	rxF, rxIsFloat := b.(float64)

	lxI, lxIsInt := a.(int64)
	rxI, rxIsInt := b.(int64)

	lxS, lxIsStr := a.(string)
	rxS, rxIsStr := b.(string)

	if lxIsFloat {
		if rxIsFloat {
			return compareFloat(lxF, rxF, opnd)
		}

		if rxIsInt {
			return compareFloat(lxF, float64(rxI), opnd)
		}

		if rxIsStr {
			rx, err := strconv.ParseFloat(rxS, 64)
			if err != nil {
				return false, fmt.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
			}

			return compareFloat(lxF, rx, opnd)
		}

		return false, fmt.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
	}

	if lxIsInt {
		if rxIsFloat {
			return compareFloat(float64(lxI), rxF, opnd)
		}

		if rxIsInt {
			return compareInt(lxI, rxI, opnd)
		}

		if rxIsStr {
			rx, err := strconv.ParseFloat(rxS, 64)
			if err != nil {
				return false, fmt.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
			}

			return compareFloat(lxF, rx, opnd)
		}

		return false, fmt.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
	}

	if lxIsStr {
		if lx, err := strconv.ParseFloat(lxS, 64); err == nil {
			return compare(lx, b, opnd)
		}

		if lx, err := strconv.ParseInt(lxS, 10, 32); err == nil {
			return compare(lx, b, opnd)
		}

		if rxIsStr {
			return compareString(lxS, rxS, opnd)
		}
	}

	return false, fmt.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
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
		b := t.Pop()
		a := t.Pop()

		match, err := compare(a, b, i.opnd.(int))
		if err != nil {
			v.errorf("%s", err)
		}

		t.match = match

	case jnm:
		if !t.match {
			t.pc = i.opnd.(int)
		}

	case jm:
		if t.match {
			t.pc = i.opnd.(int)
		}

	case jmp:
		t.pc = i.opnd.(int)

	case inc:
		// Increment a datum
		var delta int64 = 1
		// If opnd is non-nil, the delta is on the stack.
		if i.opnd != nil {
			var err error
			delta, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		// TODO(jaq): the stack should only have the datum, not the offset
		switch n := t.Pop().(type) {
		case datum.Datum:
			datum.IncIntBy(n, delta, t.time)
		case int: // offset into metric
			m := v.m[n]
			d, err := m.GetDatum()
			if err != nil {
				v.errorf("GetDatum failed: %s", err)
			}
			datum.IncIntBy(d, delta, t.time)
		default:
			v.errorf("Unexpected type to increment: %T %q", n, n)
		}

	case iset:
		// Set a datum
		value, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		// TODO(jaq): the stack should only have the datum, not the offset
		switch n := t.Pop().(type) {
		case datum.Datum:
			datum.SetInt(n, value, t.time)
		case int: // offset into metric
			m := v.m[n]
			d, err := m.GetDatum()
			if err != nil {
				v.errorf("GetDatum failed: %s", err)
			}
			datum.SetInt(d, value, t.time)
		default:
			v.errorf("Unexpected type to set: %T %q", n, n)
		}

	case fset:
		// Set a datum
		value, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		// TODO(jaq): the stack should only have the datum, not the offset, unfortunately used by test
		switch n := t.Pop().(type) {
		case datum.Datum:
			datum.SetFloat(n, value, t.time)
		case int: // offset into metric
			m := v.m[n]
			d, err := m.GetDatum()
			if err != nil {
				v.errorf("GetDatum failed: %s", err)
			}
			datum.SetFloat(d, value, t.time)
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

	case fadd, fsub, fmul, fdiv, fmod, fpow:
		b, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		switch i.op {
		case fadd:
			t.Push(a + b)
		case fsub:
			t.Push(a - b)
		case fmul:
			t.Push(a * b)
		case fdiv:
			t.Push(a / b)
		case fmod:
			t.Push(math.Mod(a, b))
		case fpow:
			t.Push(math.Pow(a, b))
		}

	case iadd, isub, imul, idiv, imod, ipow, shl, shr, and, or, xor:
		// Op two values at TOS, and push result onto stack
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		switch i.op {
		case iadd:
			t.Push(a + b)
		case isub:
			t.Push(a - b)
		case imul:
			t.Push(a * b)
		case idiv:
			// Integer division
			t.Push(a / b)
		case imod:
			t.Push(a % b)
		case ipow:
			// TODO(jaq): replace with type coercion
			t.Push(int64(math.Pow(float64(a), float64(b))))
		case shl:
			t.Push(a << uint(b))
		case shr:
			t.Push(a >> uint(b))
		case and:
			t.Push(a & b)
		case or:
			t.Push(a | b)
		case xor:
			t.Push(a ^ b)
		}

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
			v.errorf("dload (GetDatum) failed: %s", err)
		}
		//fmt.Printf("Found %v\n", d)
		t.Push(d)

	case del:
		m := t.Pop().(*metrics.Metric)
		index := i.opnd.(int)
		keys := make([]string, index)
		for j := 0; j < index; j++ {
			s := t.Pop().(string)
			keys[j] = s
		}
		err := m.RemoveDatum(keys...)
		if err != nil {
			v.errorf("del (RemoveDatum) failed: %s", err)
		}

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

	case setmatched:
		t.matched = i.opnd.(bool)

	case otherwise:
		// Only match if the matched flag is false.
		t.match = !t.matched

	default:
		v.errorf("illegal instruction: %d", i.op)
	}
}

// processLine handles the incoming lines from the input channel, by running a
// fetch-execute cycle on the VM bytecode with the line as input to the
// program, until termination.
func (v *VM) processLine(input string) {
	t := new(thread)
	t.matched = false
	v.t = t
	v.input = input
	t.stack = make([]interface{}, 0)
	t.matches = make(map[int][]string, len(v.re))
	for {
		if t.pc >= len(v.prog) {
			return
		}
		i := v.prog[t.pc]
		t.pc++
		v.execute(t, i)
		if v.terminate {
			// Terminate only stops this invocation on this line of input; reset the terminate flag.
			v.terminate = false
			return
		}
	}
}

// Run executes the virtual machine on each line of input received.  When the
// input closes, it signals to the loader that it has terminated by closing the
// shutdown channel.
func (v *VM) Run(_ uint32, lines <-chan string, shutdown chan<- struct{}) {
	glog.Infof("Starting program %s", v.name)
	defer close(shutdown)
	for line := range lines {
		v.processLine(line)
	}
	glog.Infof("Stopping program %s", v.name)
}

// New creates a new virtual machine with the given name, and compiler
// artifacts for executable and data segments.
func New(name string, obj *object, syslogUseCurrentYear bool) *VM {
	return &VM{
		name:                 name,
		re:                   obj.re,
		str:                  obj.str,
		m:                    obj.m,
		prog:                 obj.prog,
		timeMemos:            make(map[string]time.Time, 0),
		syslogUseCurrentYear: syslogUseCurrentYear,
	}
}

// DumpByteCode emits the program disassembly and program objects to string.
func (v *VM) DumpByteCode(name string) string {
	b := new(bytes.Buffer)
	fmt.Fprintf(b, "Prog: %s\n", name)
	fmt.Fprintln(b, "Metrics")
	for i, m := range v.m {
		if m.Program == v.name {
			fmt.Fprintf(b, " %8d %s\n", i, m)
		}
	}
	fmt.Fprintln(b, "Regexps")
	for i, re := range v.re {
		fmt.Fprintf(b, " %8d /%s/\n", i, re)
	}
	fmt.Fprintln(b, "Strings")
	for i, str := range v.str {
		fmt.Fprintf(b, " %8d \"%s\"\n", i, str)
	}
	w := new(tabwriter.Writer)
	w.Init(b, 0, 0, 1, ' ', tabwriter.AlignRight)

	fmt.Fprintln(w, "disasm\tl\top\topnd\t")
	for n, i := range v.prog {
		fmt.Fprintf(w, "\t%d\t%s\t%v\t\n", n, opNames[i.op], i.opnd)
	}
	w.Flush()
	return b.String()
}
