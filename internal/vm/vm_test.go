// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/bytecode"
)

var instructions = []struct {
	name          string
	i             bytecode.Instr
	re            []*regexp.Regexp
	str           []string
	reversedStack []interface{} // stack is inverted to be pushed onto vm stack

	expectedStack  []interface{}
	expectedThread thread
}{
	{"match",
		bytecode.Instr{bytecode.Match, 0},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"cmp lt",
		bytecode.Instr{bytecode.Cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq",
		bytecode.Instr{bytecode.Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2", "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp le",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ne",
		bytecode.Instr{bytecode.Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ge",
		bytecode.Instr{bytecode.Cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2.0", "1.0"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2.0"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string false",
		bytecode.Instr{bytecode.Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string true",
		bytecode.Instr{bytecode.Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "abc"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 1.0},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		bytecode.Instr{bytecode.Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jnm",
		bytecode.Instr{bytecode.Jnm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"jm",
		bytecode.Instr{bytecode.Jm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jmp",
		bytecode.Instr{bytecode.Jmp, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"strptime",
		bytecode.Instr{bytecode.Strptime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 0, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"iadd",
		bytecode.Instr{bytecode.Iadd, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"isub",
		bytecode.Instr{bytecode.Isub, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imul",
		bytecode.Instr{bytecode.Imul, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"idiv",
		bytecode.Instr{bytecode.Idiv, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod",
		bytecode.Instr{bytecode.Imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod 2",
		bytecode.Instr{bytecode.Imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3, 2},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"tolower",
		bytecode.Instr{bytecode.Tolower, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"mIxeDCasE"},
		[]interface{}{"mixedcase"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length",
		bytecode.Instr{bytecode.Length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1234"},
		[]interface{}{4},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length 0",
		bytecode.Instr{bytecode.Length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{""},
		[]interface{}{0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shl",
		bytecode.Instr{bytecode.Shl, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shr",
		bytecode.Instr{bytecode.Shr, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"and",
		bytecode.Instr{bytecode.And, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"or",
		bytecode.Instr{bytecode.Or, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor",
		bytecode.Instr{bytecode.Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 2",
		bytecode.Instr{bytecode.Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 3},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 3",
		bytecode.Instr{bytecode.Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{-1, 3},
		[]interface{}{int64(^3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"neg",
		bytecode.Instr{bytecode.Neg, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{int64(-1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"not",
		bytecode.Instr{bytecode.Not, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"pow",
		bytecode.Instr{bytecode.Ipow, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i pop",
		bytecode.Instr{bytecode.S2i, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"ff", 16},
		[]interface{}{int64(255)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i",
		bytecode.Instr{bytecode.S2i, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"190"},
		[]interface{}{int64(190)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2f",
		bytecode.Instr{bytecode.S2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0"},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2f",
		bytecode.Instr{bytecode.I2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"settime",
		bytecode.Instr{bytecode.Settime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{int64(0)},
		[]interface{}{},
		thread{pc: 0, time: time.Unix(0, 0).UTC(), matches: map[int][]string{}}},
	{"push int",
		bytecode.Instr{bytecode.Push, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1},
		thread{pc: 0, matches: map[int][]string{}}},
	{"push float",
		bytecode.Instr{bytecode.Push, 1.0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"setmatched false",
		bytecode.Instr{bytecode.Setmatched, false},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: false, pc: 0, matches: map[int][]string{}}},
	{"setmatched true",
		bytecode.Instr{bytecode.Setmatched, true},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: true, pc: 0, matches: map[int][]string{}}},
	{"otherwise",
		bytecode.Instr{bytecode.Otherwise, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fadd",
		bytecode.Instr{bytecode.Fadd, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{3.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fsub",
		bytecode.Instr{bytecode.Fsub, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{-1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmul",
		bytecode.Instr{bytecode.Fmul, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{2.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fdiv",
		bytecode.Instr{bytecode.Fdiv, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{0.5},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmod",
		bytecode.Instr{bytecode.Fmod, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fpow",
		bytecode.Instr{bytecode.Fpow, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 2.0},
		[]interface{}{4.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"getfilename",
		bytecode.Instr{bytecode.Getfilename, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{testFilename},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2s",
		bytecode.Instr{bytecode.I2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{"1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"f2s",
		bytecode.Instr{bytecode.F2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3.1},
		[]interface{}{"3.1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cat",
		bytecode.Instr{bytecode.Cat, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"first", "second"},
		[]interface{}{"firstsecond"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"icmp gt false",
		bytecode.Instr{bytecode.Icmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fcmp gt false",
		bytecode.Instr{bytecode.Fcmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"scmp eq false",
		bytecode.Instr{bytecode.Scmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
}

const testFilename = "test"

// Testbytecode.Instrs tests that each instruction behaves as expected through one
// instruction cycle.
func TestInstrs(t *testing.T) {
	for _, tc := range instructions {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			var m []*metrics.Metric
			m = append(m,
				metrics.NewMetric("foo", "test", metrics.Counter, metrics.Int),
				metrics.NewMetric("bar", "test", metrics.Counter, metrics.Int),
				metrics.NewMetric("quux", "test", metrics.Gauge, metrics.Float))
			obj := &Object{Regexps: tc.re, Strings: tc.str, Metrics: m, Program: []bytecode.Instr{tc.i}}
			v := New(tc.name, obj, true, nil)
			v.t = new(thread)
			v.t.stack = make([]interface{}, 0)
			for _, item := range tc.reversedStack {
				v.t.Push(item)
			}
			v.t.matches = make(map[int][]string)
			v.input = logline.NewLogLine(testFilename, "aaaab")
			v.execute(v.t, tc.i)
			if v.terminate {
				t.Fatalf("Execution failed, see info log.")
			}

			if diff := testutil.Diff(tc.expectedStack, v.t.stack); diff != "" {
				t.Log("unexpected vm stack state")
				t.Error(diff)
			}

			tc.expectedThread.stack = tc.expectedStack

			if diff := testutil.Diff(&tc.expectedThread, v.t, testutil.AllowUnexported(thread{})); diff != "" {
				t.Log("unexpected vm thread state")
				t.Error(diff)
				t.Errorf("\t%v", *v.t)
				t.Errorf("\t%v", tc.expectedThread)
			}

		})
	}
}

// makeVM is a helper method for construction a single-instruction VM
func makeVM(i bytecode.Instr, m []*metrics.Metric) *VM {
	obj := &Object{Metrics: m, Program: []bytecode.Instr{i}}
	v := New("test", obj, true, nil)
	v.t = new(thread)
	v.t.stack = make([]interface{}, 0)
	v.t.matches = make(map[int][]string)
	v.input = logline.NewLogLine(testFilename, "aaaab")
	return v

}

// bytecode.Instructions with datum store side effects
func TestDatumSetInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Gauge, metrics.String),
	)

	// simple inc
	v := makeVM(bytecode.Instr{bytecode.Inc, nil}, m)
	d, err := m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "1" {
		t.Errorf("Unexpected value %v", d)
	}
	// inc by int
	v = makeVM(bytecode.Instr{bytecode.Inc, 0}, m)
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push(2)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "3" {
		t.Errorf("Unexpected value %v", d)
	}
	// inc by str
	v = makeVM(bytecode.Instr{bytecode.Inc, 0}, m)
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push("1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "4" {
		t.Errorf("Unexpected value %v", d)
	}
	// iset
	v = makeVM(bytecode.Instr{bytecode.Iset, nil}, m)
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push(2)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "2" {
		t.Errorf("Unexpected value %v", d)
	}
	// iset str
	v = makeVM(bytecode.Instr{bytecode.Iset, nil}, m)
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push("3")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "3" {
		t.Errorf("Unexpected value %v", d)
	}
	// fset
	v = makeVM(bytecode.Instr{bytecode.Fset, nil}, m)
	d, err = m[1].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push(3.1)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "3.1" {
		t.Errorf("Unexpected value %v", d)
	}
	// fset str
	v = makeVM(bytecode.Instr{bytecode.Fset, nil}, m)
	d, err = m[1].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push("4.1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "4.1" {
		t.Errorf("Unexpected value %v", d)
	}

	// sset
	v = makeVM(bytecode.Instr{bytecode.Sset, nil}, m)
	d, err = m[2].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	v.t.Push(d)
	v.t.Push("4.1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "4.1" {
		t.Errorf("Unexpected value %v", d)
	}

	// dec
	v = makeVM(bytecode.Instr{bytecode.Dec, nil}, m)
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	datum.SetInt(d, 1, time.Now())
	v.t.Push(d)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	if err != nil {
		t.Fatal(err)
	}
	if d.ValueString() != "0" {
		t.Errorf("Unexpected value %v", d)
	}
}

func TestStrptimeWithTimezone(t *testing.T) {
	loc, _ := time.LoadLocation("Europe/Berlin")
	obj := &Object{Program: []bytecode.Instr{{bytecode.Strptime, 0}}}
	vm := New("strptimezone", obj, true, loc)
	vm.t = new(thread)
	vm.t.stack = make([]interface{}, 0)
	vm.t.Push("2012/01/18 06:25:00")
	vm.t.Push("2006/01/02 15:04:05")
	vm.execute(vm.t, obj.Program[0])
	if vm.t.time != time.Date(2012, 01, 18, 06, 25, 00, 00, loc) {
		t.Errorf("Time didn't parse with location: %s received", vm.t.time)
	}
}

func TestStrptimeWithoutTimezone(t *testing.T) {
	obj := &Object{Program: []bytecode.Instr{{bytecode.Strptime, 0}}}
	vm := New("strptimezone", obj, true, nil)
	vm.t = new(thread)
	vm.t.stack = make([]interface{}, 0)
	vm.t.Push("2012/01/18 06:25:00")
	vm.t.Push("2006/01/02 15:04:05")
	vm.execute(vm.t, obj.Program[0])
	if vm.t.time != time.Date(2012, 01, 18, 06, 25, 00, 00, time.UTC) {
		t.Errorf("Time didn't parse with location: %s received", vm.t.time)
	}
}

// bytecode.Instructions with datum retrieve
func TestDatumFetchInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Text, metrics.String))

	{
		// iget
		v := makeVM(bytecode.Instr{bytecode.Iget, nil}, m)
		d, err := m[0].GetDatum()
		if err != nil {
			t.Fatal(err)
		}
		datum.SetInt(d, 37, time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, err := v.t.PopInt()
		if err != nil {
			t.Fatalf("Execution failed, see info")
		}
		if i != 37 {
			t.Errorf("unexpected value %d", i)
		}
	}

	{
		// fget
		v := makeVM(bytecode.Instr{bytecode.Fget, nil}, m)
		d, err := m[1].GetDatum()
		if err != nil {
			t.Fatal(err)
		}
		datum.SetFloat(d, 12.1, time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, err := v.t.PopFloat()
		if err != nil {
			t.Fatalf("Execution failed, see info")
		}
		if i != 12.1 {
			t.Errorf("unexpected value %f", i)
		}
	}

	{
		// sget
		v := makeVM(bytecode.Instr{bytecode.Sget, nil}, m)
		d, err := m[2].GetDatum()
		if err != nil {
			t.Fatal(err)
		}
		datum.SetString(d, "aba", time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, ok := v.t.Pop().(string)
		if !ok {
			t.Fatalf("Execution failed, see info")
		}
		if i != "aba" {
			t.Errorf("unexpected value %q", i)
		}
	}
}

func TestDeleteInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int, "a"),
	)

	m[0].GetDatum("z")

	v := makeVM(bytecode.Instr{bytecode.Expire, 1}, m)
	v.t.Push(time.Hour)
	v.t.Push("z")
	v.t.Push(m[0])
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatal("execution failed, see info log")
	}
	lv := m[0].FindLabelValueOrNil([]string{"z"})
	if lv == nil {
		t.Fatalf("couldbn;t find label value in metric %#v", m[0])
	}
	if lv.Expiry != time.Hour {
		t.Fatalf("Expiry not correct, is %v", lv.Expiry)
	}
}

func TestTimestampInstr(t *testing.T) {
	var m []*metrics.Metric
	now := time.Now().UTC()
	v := makeVM(bytecode.Instr{bytecode.Timestamp, nil}, m)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatal("execution failed, see info log")
	}
	tos := time.Unix(v.t.Pop().(int64), 0).UTC()
	if now.Before(tos) {
		t.Errorf("Expecting timestamp to be after %s, was %s", now, tos)
	}

	newT := time.Unix(37, 0).UTC()
	v.t.time = newT
	v.execute(v.t, v.prog[0])

	if v.terminate {
		t.Fatal("execution failed, see info log")
	}
	tos = time.Unix(v.t.Pop().(int64), 0).UTC()
	if tos != newT {
		t.Errorf("Expecting timestamp to be %s, was %s", newT, tos)
	}
}
