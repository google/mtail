// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package vm provides a compiler and virtual machine environment for executing
// mtail programs.
package vm

import (
	"bytes"
	"context"
	"fmt"
	"math"
	"regexp"
	"runtime/debug"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/golang/glog"
	"github.com/golang/groupcache/lru"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/vm/code"
	"github.com/google/mtail/internal/vm/object"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
	"go.opencensus.io/trace"
)

var (
	lineProcessingDurations = prometheus.NewHistogramVec(prometheus.HistogramOpts{
		Namespace: "mtail",
		Subsystem: "vm",
		Name:      "line_processing_duration_seconds",
		Help:      "VM line processing time distribution in seconds.",
		Buckets:   prometheus.ExponentialBuckets(0.00002, 2.0, 10),
	}, []string{"prog"})
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
	prog []code.Instr

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
func (v *VM) execute(t *thread, i code.Instr) {
	defer func() {
		if r := recover(); r != nil {
			v.errorf("panic in thread %#v at instr %q: %s", t, i, r)
			v.abort = true
		}
	}()

	switch i.Opcode {
	case code.Bad:
		v.errorf("Invalid instruction.  Aborting.")
		v.abort = true

	case code.Stop:
		v.terminate = true

	case code.Match:
		// match regex and store success
		// Store the results in the operandth element of the stack,
		// where i.opnd == the matched re index
		index := i.Operand.(int)
		t.matches[index] = v.re[index].FindStringSubmatch(v.input.Line)
		t.Push(t.matches[index] != nil)

	case code.Smatch:
		// match regex against item on the stack
		index := i.Operand.(int)
		line := t.Pop().(string)
		t.matches[index] = v.re[index].FindStringSubmatch(line)
		t.Push(t.matches[index] != nil)

	case code.Cmp:
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

	case code.Icmp:
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
	case code.Fcmp:
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
	case code.Scmp:
		b := t.Pop().(string)
		a := t.Pop().(string)
		match, err := compareString(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
		}

		t.Push(match)

	case code.Jnm:
		match := t.Pop().(bool)
		if !match {
			t.pc = i.Operand.(int)
		}

	case code.Jm:
		match := t.Pop().(bool)
		if match {
			t.pc = i.Operand.(int)
		}

	case code.Jmp:
		t.pc = i.Operand.(int)

	case code.Inc:
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

	case code.Dec:
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

	case code.Iset:
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

	case code.Fset:
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

	case code.Sset:
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

	case code.Strptime:
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

	case code.Timestamp:
		// Put the time register onto the stack, unless it's zero in which case use system time.
		if t.time.IsZero() {
			t.Push(time.Now().Unix())
		} else {
			// Put the time register onto the stack
			t.Push(t.time.Unix())
		}

	case code.Settime:
		// Pop TOS and store in time register
		t.time = time.Unix(t.Pop().(int64), 0).UTC()

	case code.Capref:
		// Put a capture group reference onto the stack.
		// First find the match storage index on the stack,
		re := t.Pop().(int)
		// Push the result from the re'th match at operandth index
		t.Push(t.matches[re][i.Operand.(int)])

	case code.Str:
		// Put a string constant onto the stack
		t.Push(v.str[i.Operand.(int)])

	case code.Push:
		// Push a value onto the stack
		t.Push(i.Operand)

	case code.Fadd, code.Fsub, code.Fmul, code.Fdiv, code.Fmod, code.Fpow:
		b, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		a, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		switch i.Opcode {
		case code.Fadd:
			t.Push(a + b)
		case code.Fsub:
			t.Push(a - b)
		case code.Fmul:
			t.Push(a * b)
		case code.Fdiv:
			t.Push(a / b)
		case code.Fmod:
			t.Push(math.Mod(a, b))
		case code.Fpow:
			t.Push(math.Pow(a, b))
		}

	case code.Iadd, code.Isub, code.Imul, code.Idiv, code.Imod, code.Ipow, code.Shl, code.Shr, code.And, code.Or, code.Xor:
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
		case code.Iadd:
			t.Push(a + b)
		case code.Isub:
			t.Push(a - b)
		case code.Imul:
			t.Push(a * b)
		case code.Idiv:
			// Integer division
			t.Push(a / b)
		case code.Imod:
			t.Push(a % b)
		case code.Ipow:
			// TODO(jaq): replace with type coercion
			t.Push(int64(math.Pow(float64(a), float64(b))))
		case code.Shl:
			t.Push(a << uint(b))
		case code.Shr:
			t.Push(a >> uint(b))
		case code.And:
			t.Push(a & b)
		case code.Or:
			t.Push(a | b)
		case code.Xor:
			t.Push(a ^ b)
		}

	case code.Neg:
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(^a)

	case code.Not:
		a := t.Pop().(bool)
		t.Push(!a)

	case code.Mload:
		// Load a metric at operand onto stack
		t.Push(v.m[i.Operand.(int)])

	case code.Dload:
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

	case code.Iget, code.Fget, code.Sget:
		d, ok := t.Pop().(datum.Datum)
		if !ok {
			v.errorf("Unexpected value on stack: %q", d)
		}
		switch i.Opcode {
		case code.Iget:
			t.Push(datum.GetInt(d))
		case code.Fget:
			t.Push(datum.GetFloat(d))
		case code.Sget:
			t.Push(datum.GetString(d))
		}

	case code.Del:
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

	case code.Expire:
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

	case code.Tolower:
		// Lowercase code.a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(strings.ToLower(s))

	case code.Length:
		// Compute the length of a string from TOS, and push result back.
		s := t.Pop().(string)
		t.Push(len(s))

	case code.S2i:
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

	case code.S2f:
		str := t.Pop().(string)
		f, err := strconv.ParseFloat(str, 64)
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(f)

	case code.I2f:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(float64(i))

	case code.I2s:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(fmt.Sprintf("%d", i))

	case code.F2s:
		f, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
		}
		t.Push(fmt.Sprintf("%g", f))

	case code.Setmatched:
		t.matched = i.Operand.(bool)

	case code.Otherwise:
		// Only match if the matched flag is false.
		t.Push(!t.matched)

	case code.Getfilename:
		t.Push(v.input.Filename)

	case code.Cat:
		s1 := t.Pop().(string)
		s2 := t.Pop().(string)
		t.Push(s2 + s1)

	default:
		v.errorf("illegal instruction: %d", i.Opcode)
	}
}

// ProcessLogLine handles the incoming lines by running a fetch-execute cycle
// on the VM bytecode with the line as input to the program, until termination.
func (v *VM) ProcessLogLine(ctx context.Context, line *logline.LogLine) {
	ctx, span := trace.StartSpan(ctx, "VM.ProcessLogLine")
	defer span.End()
	span.AddAttributes(trace.StringAttribute("vm.prog", v.name))
	start := time.Now()
	defer func() {
		lineProcessingDurations.WithLabelValues(v.name).Observe(time.Since(start).Seconds())
	}()
	t := new(thread)
	t.matched = false
	v.t = t
	v.input = line
	t.stack = make([]interface{}, 0)
	t.matches = make(map[int][]string, len(v.re))
	_, span1 := trace.StartSpan(ctx, "execute loop")
	defer span1.End()
	for {
		if t.pc >= len(v.prog) {
			span1.AddAttributes(trace.BoolAttribute("vm.terminated", false))
			return
		}
		i := v.prog[t.pc]
		t.pc++
		v.execute(t, i)
		if v.terminate || v.abort {
			span1.AddAttributes(trace.BoolAttribute("vm.terminated", true))
			// Terminate only stops this invocation on this line of input; reset the terminate flag.
			v.terminate = false
			return
		}
	}
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
