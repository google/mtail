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
	"github.com/pkg/errors"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/vm/bytecode"
	"github.com/google/mtail/internal/vm/object"

	"github.com/golang/groupcache/lru"
)

type thread struct {
	pc      int              // Program counter.
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
	prog []bytecode.Instr

	re  []*regexp.Regexp  // Regular expression constants
	str []string          // String constants
	m   []*metrics.Metric // Metrics accessible to this program.

	timeMemos *lru.Cache // memo of time string parse results

	t *thread // Current thread of execution

	input *logline.LogLine // Log line input to this round of execution.

	terminate bool // Flag to stop the VM on this line of input.
	abort     bool // Flag to abort the VM.

	syslogUseCurrentYear bool           // Overwrite zero years with the current year in a strptime.
	loc                  *time.Location // Override local timezone with provided, if not empty
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
	progRuntimeErrors.Add(v.name, 1)
	glog.Infof(v.name+": Runtime error: "+format+"\n", args...)
	glog.Infof("VM stack:\n%s", debug.Stack())
	glog.Infof("Dumping vm state")
	glog.Infof("Name: %s", v.name)
	glog.Infof("Input: %#v", v.input)
	glog.Infof("Thread:")
	glog.Infof(" PC %v", v.t.pc-1)
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
			return 0, errors.Wrapf(err, "conversion of %q to int failed", val)
		}
		return r, nil
	case time.Time:
		return n.Unix(), nil
	case datum.Datum:
		return datum.GetInt(n), nil
	}
	return 0, errors.Errorf("unexpected int type %T %q", val, val)
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
			return 0, errors.Wrapf(err, "conversion of %q to float failed", val)
		}
		return r, nil
	case datum.Datum:
		return datum.GetFloat(n), nil
	}
	return 0, errors.Errorf("unexpected float type %T %q", val, val)
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
		return false, errors.Errorf("unexpected operator type %q", opnd)
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
		return false, errors.Errorf("unexpected operator type %q", opnd)
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
		return false, errors.Errorf("unexpected operator type %q", opnd)
	}
}

func compare(a, b interface{}, opnd int) (bool, error) {
	lxF, lxIsFloat := a.(float64)
	rxF, rxIsFloat := b.(float64)

	var n int
	var lxI, rxI int64
	var lxIsInt, rxIsInt bool

	if n, lxIsInt = a.(int); lxIsInt {
		lxI = int64(n)
	} else {
		lxI, lxIsInt = a.(int64)
	}

	if n, rxIsInt = b.(int); rxIsInt {
		rxI = int64(n)
	} else {
		rxI, rxIsInt = b.(int64)
	}

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
				return false, errors.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
			}

			return compareFloat(lxF, rx, opnd)
		}

		return false, errors.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
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
				return false, errors.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
			}

			return compareFloat(lxF, rx, opnd)
		}

		return false, errors.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
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

	return false, errors.Errorf("cannot compare %T %q with %T %q", a, a, b, b)
}

// ParseTime performs location and syslog-year aware timestamp parsing.
func (v *VM) ParseTime(layout, value string) (tm time.Time) {
	var err error
	if v.loc != nil {
		tm, err = time.ParseInLocation(layout, value, v.loc)
	} else {
		tm, err = time.Parse(layout, value)
	}
	if err != nil {
		v.errorf("strptime (%v, %v, %v) failed: %s", layout, value, v.loc, err)
		return
	}
	// Hack for yearless syslog.
	if tm.Year() == 0 && v.syslogUseCurrentYear {
		// No .UTC() as we use local time to match the local log.
		now := time.Now()
		// unless there's a timezone
		if v.loc != nil {
			now = now.In(v.loc)
		}
		tm = tm.AddDate(now.Year(), 0, 0)
	}
	return
}

// execute performs an instruction cycle in the VM. acting on the instruction
// i in thread t.
func (v *VM) execute(t *thread, i bytecode.Instr) {
	defer func() {
		if r := recover(); r != nil {
			v.errorf("panic in thread %#v at instr %q: %s", t, i, r)
			v.abort = true
		}
	}()

	switch i.Opcode {
	case bytecode.Bad:
		v.errorf("Invalid instruction.  Aborting.")
		v.abort = true

	case bytecode.Stop:
		v.terminate = true

	case bytecode.Match:
		// match regex and store success
		// Store the results in the operandth element of the stack,
		// where i.opnd == the matched re index
		index := i.Operand.(int)
		t.matches[index] = v.re[index].FindStringSubmatch(v.input.Line)
		t.Push(t.matches[index] != nil)

	case bytecode.Smatch:
		// match regex against item on the stack
		index := i.Operand.(int)
		line := t.Pop().(string)
		t.matches[index] = v.re[index].FindStringSubmatch(line)
		t.Push(t.matches[index] != nil)

	case bytecode.Cmp:
		// Compare two elements on the stack.
		// Set the match register based on the truthiness of the comparison.
		// Operand contains the expected result.
		b := t.Pop()
		a := t.Pop()

		match, err := compare(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
		}

		t.Push(match)

	case bytecode.Icmp:
		b, berr := t.PopInt()
		if berr != nil {
			v.errorf("%v", berr)
		}
		a, aerr := t.PopInt()
		if aerr != nil {
			v.errorf("%v", aerr)
		}
		match, err := compareInt(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
		}

		t.Push(match)
	case bytecode.Fcmp:
		b, berr := t.PopFloat()
		if berr != nil {
			v.errorf("%v", berr)
		}
		a, aerr := t.PopFloat()
		if aerr != nil {
			v.errorf("%v", aerr)
		}
		match, err := compareFloat(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
		}

		t.Push(match)
	case bytecode.Scmp:
		b := t.Pop().(string)
		a := t.Pop().(string)
		match, err := compareString(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
		}

		t.Push(match)

	case bytecode.Jnm:
		match := t.Pop().(bool)
		if !match {
			t.pc = i.Operand.(int)
		}

	case bytecode.Jm:
		match := t.Pop().(bool)
		if match {
			t.pc = i.Operand.(int)
		}

	case bytecode.Jmp:
		t.pc = i.Operand.(int)

	case bytecode.Inc:
		// Increment a datum
		var delta int64 = 1
		// If opnd is non-nil, the delta is on the stack.
		if i.Operand != nil {
			var err error
			delta, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.IncIntBy(n, delta, t.time)
		} else {
			v.errorf("Unexpected type to increment: %T %q", n, n)
		}

	case bytecode.Dec:
		// Decrement a datum
		var delta int64 = 1
		// If opnd is non-nil, the delta is on the stack.
		if i.Operand != nil {
			var err error
			delta, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.DecIntBy(n, delta, t.time)
		} else {
			v.errorf("Unexpected type to increment: %T %q", n, n)
		}

	case bytecode.Iset:
		// Set a datum
		value, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetInt(n, value, t.time)
		} else {
			v.errorf("Unexpected type to iset: %T %q", n, n)
		}

	case bytecode.Fset:
		// Set a datum
		value, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetFloat(n, value, t.time)
		} else {
			v.errorf("Unexpected type to fset: %T %q", n, n)
		}

	case bytecode.Sset:
		// Set a string datum
		value, ok := t.Pop().(string)
		if !ok {
			v.errorf("Value on stack was not a string: %T %q", value, value)
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetString(n, value, t.time)
		} else {
			v.errorf("Unexpected type to sset: %T %q", n, n)
		}

	case bytecode.Strptime:
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
		if cached, ok := v.timeMemos.Get(ts); !ok {
			tm := v.ParseTime(layout, ts)
			v.timeMemos.Add(ts, tm)
			t.time = tm
		} else {
			t.time = cached.(time.Time)
		}

	case bytecode.Timestamp:
		// Put the time register onto the stack, unless it's zero in which case bytecode.use system time.
		if t.time.IsZero() {
			t.Push(time.Now().Unix())
		} else {
			// Put the time register onto the stack
			t.Push(t.time.Unix())
		}

	case bytecode.Settime:
		// Pop TOS and store in time register
		t.time = time.Unix(t.Pop().(int64), 0).UTC()

	case bytecode.Capref:
		// Put a capture group reference onto the stack.
		// First find the match storage index on the stack,
		re := t.Pop().(int)
		// Push the result from the re'th match at operandth index
		t.Push(t.matches[re][i.Operand.(int)])

	case bytecode.Str:
		// Put a string constant onto the stack
		t.Push(v.str[i.Operand.(int)])

	case bytecode.Push:
		// Push a value onto the stack
		t.Push(i.Operand)

	case bytecode.Fadd, bytecode.Fsub, bytecode.Fmul, bytecode.Fdiv, bytecode.Fmod, bytecode.Fpow:
		b, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		switch i.Opcode {
		case bytecode.Fadd:
			t.Push(a + b)
		case bytecode.Fsub:
			t.Push(a - b)
		case bytecode.Fmul:
			t.Push(a * b)
		case bytecode.Fdiv:
			t.Push(a / b)
		case bytecode.Fmod:
			t.Push(math.Mod(a, b))
		case bytecode.Fpow:
			t.Push(math.Pow(a, b))
		}

	case bytecode.Iadd, bytecode.Isub, bytecode.Imul, bytecode.Idiv, bytecode.Imod, bytecode.Ipow, bytecode.Shl, bytecode.Shr, bytecode.And, bytecode.Or, bytecode.Xor:
		// Op two values at TOS, and push result onto stack
		b, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		switch i.Opcode {
		case bytecode.Iadd:
			t.Push(a + b)
		case bytecode.Isub:
			t.Push(a - b)
		case bytecode.Imul:
			t.Push(a * b)
		case bytecode.Idiv:
			// Integer division
			t.Push(a / b)
		case bytecode.Imod:
			t.Push(a % b)
		case bytecode.Ipow:
			// TODO(jaq): replace with type coercion
			t.Push(int64(math.Pow(float64(a), float64(b))))
		case bytecode.Shl:
			t.Push(a << uint(b))
		case bytecode.Shr:
			t.Push(a >> uint(b))
		case bytecode.And:
			t.Push(a & b)
		case bytecode.Or:
			t.Push(a | b)
		case bytecode.Xor:
			t.Push(a ^ b)
		}

	case bytecode.Neg:
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(^a)

	case bytecode.Not:
		a := t.Pop().(bool)
		t.Push(!a)

	case bytecode.Mload:
		// Load a metric at operand onto stack
		t.Push(v.m[i.Operand.(int)])

	case bytecode.Dload:
		// Load a datum from metric at TOS onto stack
		//fmt.Printf("Stack: %v\n", t.stack)
		m := t.Pop().(*metrics.Metric)
		//fmt.Printf("Metric: %v\n", m)
		index := i.Operand.(int)
		keys := make([]string, index)
		//fmt.Printf("keys: %v\n", keys)
		for a := index - 1; a >= 0; a-- {
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

	case bytecode.Iget, bytecode.Fget, bytecode.Sget:
		d, ok := t.Pop().(datum.Datum)
		if !ok {
			v.errorf("Unexpected value on stack: %q", d)
		}
		switch i.Opcode {
		case bytecode.Iget:
			t.Push(datum.GetInt(d))
		case bytecode.Fget:
			t.Push(datum.GetFloat(d))
		case bytecode.Sget:
			t.Push(datum.GetString(d))
		}

	case bytecode.Del:
		m := t.Pop().(*metrics.Metric)
		index := i.Operand.(int)
		keys := make([]string, index)
		for j := index - 1; j >= 0; j-- {
			s := t.Pop().(string)
			keys[j] = s
		}
		err := m.RemoveDatum(keys...)
		if err != nil {
			v.errorf("del (RemoveDatum) failed: %s", err)
		}

	case bytecode.Expire:
		m := t.Pop().(*metrics.Metric)
		index := i.Operand.(int)
		keys := make([]string, index)
		for j := index - 1; j >= 0; j-- {
			s := t.Pop().(string)
			keys[j] = s
		}
		expiry := t.Pop().(time.Duration)
		if err := m.ExpireDatum(expiry, keys...); err != nil {
			v.errorf("%s", err)
		}

	case bytecode.Tolower:
		// Lowercase bytecode.a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(strings.ToLower(s))

	case bytecode.Length:
		// Compute the length of a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(len(s))

	case bytecode.S2i:
		base := int64(10)
		var err error
		if i.Operand != nil {
			// strtol is emitted with an arglen, int is not
			base, err = t.PopInt()
			if err != nil {
				v.errorf("%s", err)
			}
		}
		str := t.Pop().(string)
		i, err := strconv.ParseInt(str, int(base), 64)
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(i)

	case bytecode.S2f:
		str := t.Pop().(string)
		f, err := strconv.ParseFloat(str, 64)
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(f)

	case bytecode.I2f:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(float64(i))

	case bytecode.I2s:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(fmt.Sprintf("%d", i))

	case bytecode.F2s:
		f, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(fmt.Sprintf("%g", f))

	case bytecode.Setmatched:
		t.matched = i.Operand.(bool)

	case bytecode.Otherwise:
		// Only match if the matched flag is false.
		t.Push(!t.matched)

	case bytecode.Getfilename:
		t.Push(v.input.Filename)

	case bytecode.Cat:
		s1 := t.Pop().(string)
		s2 := t.Pop().(string)
		t.Push(s2 + s1)

	default:
		v.errorf("illegal instruction: %d", i.Opcode)
	}
}

// processLine handles the incoming lines from the input channel, by running a
// fetch-execute cycle on the VM bytecode with the line as input to the
// program, until termination.
func (v *VM) processLine(line *logline.LogLine) {
	t := new(thread)
	t.matched = false
	v.t = t
	v.input = line
	t.stack = make([]interface{}, 0)
	t.matches = make(map[int][]string, len(v.re))
	for {
		if t.pc >= len(v.prog) {
			return
		}
		i := v.prog[t.pc]
		t.pc++
		v.execute(t, i)
		if v.terminate || v.abort {
			// Terminate only stops this invocation on this line of input; reset the terminate flag.
			v.terminate = false
			return
		}
	}
}

// Run executes the virtual machine on each line of input received.  When the
// input closes, it signals to the loader that it has terminated by closing the
// shutdown channel.
func (v *VM) Run(_ uint32, lines <-chan *logline.LogLine, shutdown chan<- struct{}, started chan<- struct{}) {
	defer close(shutdown)

	glog.Infof("Starting program %s", v.name)
	close(started)
	for line := range lines {
		// TODO(jaq): measure and export the processLine runtime per VM as a histo.
		v.processLine(line)
	}
	glog.Infof("Stopping program %s", v.name)
}

// New creates a new virtual machine with the given name, and compiler
// artifacts for executable and data segments.
func New(name string, obj *object.Object, syslogUseCurrentYear bool, loc *time.Location) *VM {
	return &VM{
		name:                 name,
		re:                   obj.Regexps,
		str:                  obj.Strings,
		m:                    obj.Metrics,
		prog:                 obj.Program,
		timeMemos:            lru.New(64),
		syslogUseCurrentYear: syslogUseCurrentYear,
		loc:                  loc,
	}
}

// DumpByteCode emits the program disassembly and program objects to a string.
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
		fmt.Fprintf(w, "\t%d\t%s\t%v\t\n", n, i.Opcode, i.Operand)
	}
	if err := w.Flush(); err != nil {
		glog.Infof("flush error: %s", err)
	}
	return b.String()
}
