// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package vm provides a virtual machine environment for executing
// mtail bytecode.
package vm

import (
	"bytes"
	"context"
	"expvar"
	"fmt"
	"math"
	"regexp"
	"runtime/debug"
	"strconv"
	"strings"
	"sync"
	"text/tabwriter"
	"time"

	"github.com/golang/glog"
	"github.com/golang/groupcache/lru"
	"github.com/jaqx0r/mtail/internal/logline"
	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/runtime/code"
	"github.com/pkg/errors"
	"github.com/prometheus/client_golang/prometheus"
)

var (
	ProgRuntimeErrors = expvar.NewMap("prog_runtime_errors_total")

	LineProcessingDurations = prometheus.NewHistogramVec(prometheus.HistogramOpts{
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

	re      []*regexp.Regexp  // Regular expression constants
	str     []string          // String constants
	Metrics []*metrics.Metric // Metrics accessible to this program.

	timeMemos *lru.Cache // memo of time string parse results

	t *thread // Current thread of execution

	input *logline.LogLine // Log line input to this round of execution.

	terminate bool // Flag to stop the VM on this line of input.

	HardCrash bool // User settable flag to make the VM crash instead of recover on panic.

	runtimeErrorMu sync.RWMutex // protects runtimeError
	runtimeError   string       // records the last runtime error from errorf()

	logRuntimeErrors     bool           // Emit runtime errors to the log.
	syslogUseCurrentYear bool           // Overwrite zero years with the current year in a strptime.
	loc                  *time.Location // Override local timezone with provided, if not empty.
	trace                []int          // Record program counter in program execution, for testing.
}

// Push a value onto the stack.
func (t *thread) Push(value interface{}) {
	t.stack = append(t.stack, value)
}

// Pop a value off the stack.
func (t *thread) Pop() (value interface{}) {
	last := len(t.stack) - 1
	value = t.stack[last]
	t.stack = t.stack[:last]
	return
}

// Log a runtime error and terminate the program.
func (v *VM) errorf(format string, args ...interface{}) {
	i := v.prog[v.t.pc-1]
	ProgRuntimeErrors.Add(v.name, 1)
	v.runtimeErrorMu.Lock()
	v.runtimeError = fmt.Sprintf(format+"\n", args...)
	v.runtimeError += fmt.Sprintf(
		"Error occurred at instruction %d {%s, %v}, originating in %s at line %d\n",
		v.t.pc-1, i.Opcode, i.Operand, v.name, i.SourceLine+1)
	v.runtimeError += fmt.Sprintf("Full input text from %q was %q", v.input.Filename, v.input.Line)
	if v.logRuntimeErrors || bool(glog.V(1)) {
		glog.Info(v.name + ": Runtime error: " + v.runtimeError)

		glog.Infof("Set logging verbosity higher (-v1 or more) to see full VM state dump.")
	}
	if glog.V(1) {
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
		glog.Infof(v.DumpByteCode())
	}
	if v.trace != nil {
		glog.Infof("Execution Trace: %v", v.trace)
	}
	v.runtimeErrorMu.Unlock()
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

func (t *thread) PopString() (string, error) {
	val := t.Pop()
	switch n := val.(type) {
	case string:
		return n, nil
	case float64:
		return strconv.FormatFloat(n, 'G', -1, 64), nil
	case int:
		return strconv.Itoa(n), nil
	case int64:
		return strconv.FormatInt(n, 10), nil
	case datum.Datum:
		return datum.GetString(n), nil
	}
	return "", errors.Errorf("unexpected type for string %T %q", val, val)
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
	// In normal operation, recover from panics, otherwise dump that state and repanic.
	defer func() {
		if r := recover(); r != nil {
			if v.HardCrash {
				fmt.Printf("panic in thread %#v at instr %q: %s\n", t, i, r)
				panic(r)
			}
			v.errorf("panic in thread %#v at instr %q: %s", t, i, r)
			v.terminate = true
		}
	}()

	switch i.Opcode {
	case code.Bad:
		panic("Invalid instruction.  Aborting.")

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
		line, err := t.PopString()
		if err != nil {
			v.errorf("+%v", err)
			return
		}
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
			return
		}

		t.Push(match)

	case code.Icmp:
		b, berr := t.PopInt()
		if berr != nil {
			v.errorf("%+v", berr)
			return
		}
		a, aerr := t.PopInt()
		if aerr != nil {
			v.errorf("%+v", aerr)
			return
		}
		match, err := compareInt(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
			return
		}

		t.Push(match)
	case code.Fcmp:
		b, berr := t.PopFloat()
		if berr != nil {
			v.errorf("%+v", berr)
			return
		}
		a, aerr := t.PopFloat()
		if aerr != nil {
			v.errorf("%+v", aerr)
		}
		match, err := compareFloat(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
			return
		}

		t.Push(match)
	case code.Scmp:
		b, berr := t.PopString()
		if berr != nil {
			v.errorf("%+v", berr)
			return
		}
		a, aerr := t.PopString()
		if aerr != nil {
			v.errorf("%+v", aerr)
			return
		}
		match, err := compareString(a, b, i.Operand.(int))
		if err != nil {
			v.errorf("%+v", err)
			return
		}

		t.Push(match)

	case code.Jnm:
		val := t.Pop()
		switch match := val.(type) {
		case bool:
			if !match {
				t.pc = i.Operand.(int)
			}
		case int64:
			if match == 0 {
				t.pc = i.Operand.(int)
			}
		}

	case code.Jm:
		val := t.Pop()
		switch match := val.(type) {
		case bool:
			if match {
				t.pc = i.Operand.(int)
			}
		case int64:
			if match != 0 {
				t.pc = i.Operand.(int)
			}
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
				return
			}
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.IncIntBy(n, delta, t.time)
			t.Push(datum.GetInt(n))
		} else {
			v.errorf("Unexpected type to increment: %T %q", n, n)
			return
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
				return
			}
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.DecIntBy(n, delta, t.time)
			t.Push(datum.GetInt(n))
		} else {
			v.errorf("Unexpected type to increment: %T %q", n, n)
			return
		}

	case code.Iset:
		// Set a datum
		value, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
			return
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetInt(n, value, t.time)
		} else {
			v.errorf("Unexpected type to iset: %T %q", n, n)
			return
		}

	case code.Fset:
		// Set a datum
		value, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
			return
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetFloat(n, value, t.time)
		} else {
			v.errorf("Unexpected type to fset: %T %q", n, n)
			return
		}

	case code.Sset:
		// Set a string datum
		value, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}
		if n, ok := t.Pop().(datum.Datum); ok {
			datum.SetString(n, value, t.time)
		} else {
			v.errorf("Unexpected type to sset: %T %q", n, n)
			return
		}

	case code.Strptime:
		// Parse a time string into the time register
		layout, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}

		var ts string
		switch s := t.Pop().(type) {
		case string:
			ts = s

		case int: /* capref */
			// First find the match storage index on the stack
			val, err := t.PopInt()
			if err != nil {
				v.errorf("%s", err)
				return
			}
			if val < 0 || val >= math.MaxInt32 {
				v.errorf("int32 index out of range")
				return
			}
			re := int(val)
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
		val := t.Pop()
		ts, ok := val.(int64)
		if !ok {
			v.errorf("Failed to pop a timestamp off the stack: %v instead", v)
			return
		}
		t.time = time.Unix(ts, 0).UTC()

	case code.Capref:
		// Put a capture group reference onto the stack.
		// First find the match storage index on the stack,
		val := t.Pop()
		re, ok := val.(int)
		if !ok {
			v.errorf("Invalid re index %v, not an int", val)
			return
		}
		// Push the result from the re'th match at operandth index
		op, ok := i.Operand.(int)
		if !ok {
			v.errorf("Invalid operand %v, not an int", i.Operand)
			return
		}
		if len(t.matches[re]) <= op {
			v.errorf("Not enough capture groups matched from %v to select %dth", t.matches[re], op)
			return
		}
		t.Push(t.matches[re][op])

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
			return
		}
		a, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
			return
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
			return
		}
		a, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
			return
		}
		switch i.Opcode {
		case code.Iadd:
			t.Push(a + b)
		case code.Isub:
			t.Push(a - b)
		case code.Imul:
			t.Push(a * b)
		case code.Idiv:
			if b == 0 {
				v.errorf("Divide by zero %d %% %d", a, b)
				return
			}
			// Integer division
			t.Push(a / b)
		case code.Imod:
			if b == 0 {
				v.errorf("Divide by zero %d %% %d", a, b)
				return
			}
			t.Push(a % b)
		case code.Ipow:
			// TODO(jaq): replace with type coercion
			t.Push(int64(math.Pow(float64(a), float64(b))))
		case code.Shl:
			if b < 0 || b >= math.MaxInt32 {
				v.errorf("shift int out of range")
				return
			}
			t.Push(a << uint(b))
		case code.Shr:
			if b < 0 || b >= math.MaxInt32 {
				v.errorf("shift int out of range")
				return
			}
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
			return
		}
		t.Push(^a)

	case code.Not:
		a := t.Pop().(bool)
		t.Push(!a)

	case code.Mload:
		// Load a metric at operand onto stack
		t.Push(v.Metrics[i.Operand.(int)])

	case code.Dload:
		// Load a datum from metric at TOS onto stack
		// fmt.Printf("Stack: %v\n", t.stack)
		m := t.Pop().(*metrics.Metric)
		// fmt.Printf("Metric: %v\n", m)
		index := i.Operand.(int)
		keys := make([]string, index)
		// fmt.Printf("keys: %v\n", keys)
		for a := index - 1; a >= 0; a-- {
			s, err := t.PopString()
			if err != nil {
				v.errorf("%+v", err)
				return
			}
			// fmt.Printf("s: %v\n", s)
			keys[a] = s
			// fmt.Printf("Keys: %v\n", keys)
		}
		// fmt.Printf("Keys: %v\n", keys)
		d, err := m.GetDatum(keys...)
		if err != nil {
			v.errorf("dload (GetDatum) failed: %s", err)
			return
		}
		// fmt.Printf("Found %v\n", d)
		t.Push(d)

	case code.Iget, code.Fget, code.Sget:
		d, ok := t.Pop().(datum.Datum)
		if !ok {
			v.errorf("Unexpected value on stack: %q", d)
			return
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
			s, err := t.PopString()
			if err != nil {
				v.errorf("%+v", err)
				return
			}
			keys[j] = s
		}
		err := m.RemoveDatum(keys...)
		if err != nil {
			v.errorf("del (RemoveDatum) failed: %s", err)
			return
		}

	case code.Expire:
		m := t.Pop().(*metrics.Metric)
		index := i.Operand.(int)
		keys := make([]string, index)
		for j := index - 1; j >= 0; j-- {
			s, err := t.PopString()
			if err != nil {
				v.errorf("%+v", err)
				return
			}
			keys[j] = s
		}
		expiry := t.Pop().(time.Duration)
		if err := m.ExpireDatum(expiry, keys...); err != nil {
			v.errorf("%s", err)
			return
		}

	case code.Tolower:
		// Lowercase code.a string from TOS, and push result back.
		s, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}
		t.Push(strings.ToLower(s))

	case code.Length:
		// Compute the length of a string from TOS, and push result back.
		s, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}
		t.Push(len(s))

	case code.S2i:
		base := 10
		var err error
		if i.Operand != nil {
			// strtol is emitted with an arglen, int is not
			val, err := t.PopInt()
			if err != nil {
				v.errorf("%s", err)
				return
			}
			if val <= 0 || val >= math.MaxInt32 {
				v.errorf("int32 out of range")
				return
			}
			base = int(val)
		}
		str, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}
		val, err := strconv.ParseInt(str, base, 64)
		if err != nil {
			v.errorf("%s", err)
			return
		}
		t.Push(val)

	case code.S2f:
		str, err := t.PopString()
		if err != nil {
			v.errorf("%+v", err)
			return
		}
		f, err := strconv.ParseFloat(str, 64)
		if err != nil {
			v.errorf("%s", err)
			return
		}
		t.Push(f)

	case code.I2f:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
			return
		}
		t.Push(float64(i))

	case code.I2s:
		i, err := t.PopInt()
		if err != nil {
			v.errorf("%s", err)
			return
		}
		t.Push(fmt.Sprintf("%d", i))

	case code.F2s:
		f, err := t.PopFloat()
		if err != nil {
			v.errorf("%s", err)
			return
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
		b, berr := t.PopString()
		if berr != nil {
			v.errorf("%+v", berr)
			return
		}
		a, aerr := t.PopString()
		if aerr != nil {
			v.errorf("%+v", aerr)
			return
		}
		t.Push(a + b)

	case code.Subst:
		val, verr := t.PopString()
		if verr != nil {
			v.errorf("%+v", verr)
			return
		}
		repl, nerr := t.PopString()
		if nerr != nil {
			v.errorf("%+v", nerr)
			return
		}
		old, oerr := t.PopString()
		if oerr != nil {
			v.errorf("%+v", oerr)
			return
		}
		t.Push(strings.ReplaceAll(val, old, repl))
	case code.Rsubst:
		pat, perr := t.PopInt()
		if perr != nil {
			v.errorf("%+v", perr)
			return
		}
		val, verr := t.PopString()
		if verr != nil {
			v.errorf("%+v", verr)
			return
		}
		repl, nerr := t.PopString()
		if nerr != nil {
			v.errorf("%+v", nerr)
			return
		}
		t.Push(v.re[pat].ReplaceAllLiteralString(val, repl))

	default:
		v.errorf("illegal instruction: %d", i.Opcode)
	}
}

// ProcessLogLine handles the incoming lines by running a fetch-execute cycle
// on the VM bytecode with the line as input to the program, until termination.
func (v *VM) ProcessLogLine(_ context.Context, line *logline.LogLine) {
	start := time.Now()
	defer func() {
		LineProcessingDurations.WithLabelValues(v.name).Observe(time.Since(start).Seconds())
	}()
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
		if v.trace != nil {
			v.trace = append(v.trace, t.pc)
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

// New creates a new virtual machine with the given name, and compiler
// artifacts for executable and data segments.
func New(name string, obj *code.Object, syslogUseCurrentYear bool, loc *time.Location, log bool, trace bool) *VM {
	v := &VM{
		name:                 name,
		re:                   obj.Regexps,
		str:                  obj.Strings,
		Metrics:              obj.Metrics,
		prog:                 obj.Program,
		timeMemos:            lru.New(64),
		syslogUseCurrentYear: syslogUseCurrentYear,
		loc:                  loc,
		logRuntimeErrors:     log,
	}
	if trace {
		v.trace = make([]int, 0, len(v.prog))
	}
	return v
}

// DumpByteCode emits the program disassembly and program objects to a string.
func (v *VM) DumpByteCode() string {
	b := new(bytes.Buffer)
	fmt.Fprintf(b, "Prog: %s\n", v.name)
	fmt.Fprintln(b, "Metrics")
	for i, m := range v.Metrics {
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

	fmt.Fprintln(w, "disasm\tl\top\topnd\tline\t")
	for n, i := range v.prog {
		fmt.Fprintf(w, "\t%d\t%s\t%v\t%d\t\n", n, i.Opcode, i.Operand, i.SourceLine+1)
	}
	if err := w.Flush(); err != nil {
		glog.Infof("flush error: %s", err)
	}
	return b.String()
}

// RuntimeErrorString returns the last runtime erro rthat the program enountered.
func (v *VM) RuntimeErrorString() string {
	v.runtimeErrorMu.RLock()
	defer v.runtimeErrorMu.RUnlock()
	return v.runtimeError
}

// Run starts the VM and processes lines coming in on the input channel.  When
// the channel is closed, and the VM has finished processing the VM is shut
// down and the loader signalled via the given waitgroup.
func (v *VM) Run(lines <-chan *logline.LogLine, wg *sync.WaitGroup) {
	defer wg.Done()
	glog.V(1).Infof("started VM %q", v.name)
	ctx := context.TODO()
	for line := range lines {
		v.ProcessLogLine(ctx, line)
	}
	glog.Infof("VM %q finished", v.name)
}
