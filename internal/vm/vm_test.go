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
)

var instructions = []struct {
	name          string
	i             Instr
	re            []*regexp.Regexp
	str           []string
	reversedStack []interface{} // stack is inverted to be pushed onto vm stack

	expectedStack  []interface{}
	expectedThread thread
}{
	{"match",
		Instr{Match, 0},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"cmp lt",
		Instr{Cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq",
		Instr{Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2", "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp le",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ne",
		Instr{Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ge",
		Instr{Cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2.0", "1.0"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2.0"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string false",
		Instr{Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string true",
		Instr{Cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "abc"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 1.0},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		Instr{Cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jnm",
		Instr{Jnm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"jm",
		Instr{Jm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jmp",
		Instr{Jmp, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"strptime",
		Instr{Strptime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 0, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"iadd",
		Instr{Iadd, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"isub",
		Instr{Isub, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imul",
		Instr{Imul, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"idiv",
		Instr{Idiv, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod",
		Instr{Imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod 2",
		Instr{Imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3, 2},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"tolower",
		Instr{Tolower, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"mIxeDCasE"},
		[]interface{}{"mixedcase"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length",
		Instr{Length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1234"},
		[]interface{}{4},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length 0",
		Instr{Length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{""},
		[]interface{}{0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shl",
		Instr{Shl, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shr",
		Instr{Shr, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"and",
		Instr{And, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"or",
		Instr{Or, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor",
		Instr{Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 2",
		Instr{Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 3},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 3",
		Instr{Xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{-1, 3},
		[]interface{}{int64(^3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"neg",
		Instr{Neg, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{int64(-1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"not",
		Instr{Not, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"pow",
		Instr{Ipow, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i pop",
		Instr{S2i, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"ff", 16},
		[]interface{}{int64(255)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i",
		Instr{S2i, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"190"},
		[]interface{}{int64(190)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2f",
		Instr{S2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0"},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2f",
		Instr{I2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"settime",
		Instr{Settime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{int64(0)},
		[]interface{}{},
		thread{pc: 0, time: time.Unix(0, 0).UTC(), matches: map[int][]string{}}},
	{"push int",
		Instr{Push, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1},
		thread{pc: 0, matches: map[int][]string{}}},
	{"push float",
		Instr{Push, 1.0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"setmatched false",
		Instr{Setmatched, false},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: false, pc: 0, matches: map[int][]string{}}},
	{"setmatched true",
		Instr{Setmatched, true},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: true, pc: 0, matches: map[int][]string{}}},
	{"otherwise",
		Instr{Otherwise, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fadd",
		Instr{Fadd, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{3.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fsub",
		Instr{Fsub, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{-1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmul",
		Instr{Fmul, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{2.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fdiv",
		Instr{Fdiv, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{0.5},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmod",
		Instr{Fmod, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fpow",
		Instr{Fpow, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 2.0},
		[]interface{}{4.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"getfilename",
		Instr{Getfilename, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{testFilename},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2s",
		Instr{I2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{"1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"f2s",
		Instr{F2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3.1},
		[]interface{}{"3.1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cat",
		Instr{Cat, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"first", "second"},
		[]interface{}{"firstsecond"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"icmp gt false",
		Instr{Icmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fcmp gt false",
		Instr{Fcmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"scmp eq false",
		Instr{Scmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
}

const testFilename = "test"

// TestInstrs tests that each instruction behaves as expected through one
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
			obj := &object{re: tc.re, str: tc.str, m: m, prog: []Instr{tc.i}}
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
func makeVM(i Instr, m []*metrics.Metric) *VM {
	obj := &object{m: m, prog: []Instr{i}}
	v := New("test", obj, true, nil)
	v.t = new(thread)
	v.t.stack = make([]interface{}, 0)
	v.t.matches = make(map[int][]string)
	v.input = logline.NewLogLine(testFilename, "aaaab")
	return v

}

// Instructions with datum store side effects
func TestDatumSetInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Gauge, metrics.String),
	)

	// simple inc
	v := makeVM(Instr{Inc, nil}, m)
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
	v = makeVM(Instr{Inc, 0}, m)
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
	v = makeVM(Instr{Inc, 0}, m)
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
	v = makeVM(Instr{Iset, nil}, m)
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
	v = makeVM(Instr{Iset, nil}, m)
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
	v = makeVM(Instr{Fset, nil}, m)
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
	v = makeVM(Instr{Fset, nil}, m)
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
	v = makeVM(Instr{Sset, nil}, m)
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
	v = makeVM(Instr{Dec, nil}, m)
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
	obj := &object{prog: []Instr{{Strptime, 0}}}
	vm := New("strptimezone", obj, true, loc)
	vm.t = new(thread)
	vm.t.stack = make([]interface{}, 0)
	vm.t.Push("2012/01/18 06:25:00")
	vm.t.Push("2006/01/02 15:04:05")
	vm.execute(vm.t, obj.prog[0])
	if vm.t.time != time.Date(2012, 01, 18, 06, 25, 00, 00, loc) {
		t.Errorf("Time didn't parse with location: %s received", vm.t.time)
	}
}

func TestStrptimeWithoutTimezone(t *testing.T) {
	obj := &object{prog: []Instr{{Strptime, 0}}}
	vm := New("strptimezone", obj, true, nil)
	vm.t = new(thread)
	vm.t.stack = make([]interface{}, 0)
	vm.t.Push("2012/01/18 06:25:00")
	vm.t.Push("2006/01/02 15:04:05")
	vm.execute(vm.t, obj.prog[0])
	if vm.t.time != time.Date(2012, 01, 18, 06, 25, 00, 00, time.UTC) {
		t.Errorf("Time didn't parse with location: %s received", vm.t.time)
	}
}

// Instructions with datum retrieve
func TestDatumFetchInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Text, metrics.String))

	{
		// iget
		v := makeVM(Instr{Iget, nil}, m)
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
		v := makeVM(Instr{Fget, nil}, m)
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
		v := makeVM(Instr{Sget, nil}, m)
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

	v := makeVM(Instr{Expire, 1}, m)
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
	v := makeVM(Instr{Timestamp, nil}, m)
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
