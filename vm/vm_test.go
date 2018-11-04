// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"regexp"
	"testing"
	"time"

	go_cmp "github.com/google/go-cmp/cmp"
	"github.com/google/mtail/logline"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

var instructions = []struct {
	name          string
	i             instr
	re            []*regexp.Regexp
	str           []string
	reversedStack []interface{} // stack is inverted to be pushed onto vm stack

	expectedStack  []interface{}
	expectedThread thread
}{
	{"match",
		instr{match, 0},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"cmp lt",
		instr{cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2", "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp le",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ne",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ge",
		instr{cmp, -1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2.0", "1.0"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2.0"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string false",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string true",
		instr{cmp, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "abc"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 1.0},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		instr{cmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jnm",
		instr{jnm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"jm",
		instr{jm, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jmp",
		instr{jmp, 37},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"strptime",
		instr{strptime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 0, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"iadd",
		instr{iadd, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"isub",
		instr{isub, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imul",
		instr{imul, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"idiv",
		instr{idiv, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod",
		instr{imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod 2",
		instr{imod, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3, 2},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"tolower",
		instr{tolower, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"mIxeDCasE"},
		[]interface{}{"mixedcase"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length",
		instr{length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1234"},
		[]interface{}{4},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length 0",
		instr{length, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{""},
		[]interface{}{0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shl",
		instr{shl, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shr",
		instr{shr, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"and",
		instr{and, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"or",
		instr{or, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor",
		instr{xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 2",
		instr{xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 3},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 3",
		instr{xor, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{-1, 3},
		[]interface{}{int64(^3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"neg",
		instr{neg, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{int64(-1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"not",
		instr{not, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"pow",
		instr{ipow, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i pop",
		instr{s2i, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"ff", 16},
		[]interface{}{int64(255)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i",
		instr{s2i, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"190"},
		[]interface{}{int64(190)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2f",
		instr{s2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0"},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2f",
		instr{i2f, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"settime",
		instr{settime, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{int64(0)},
		[]interface{}{},
		thread{pc: 0, time: time.Unix(0, 0).UTC(), matches: map[int][]string{}}},
	{"push int",
		instr{push, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1},
		thread{pc: 0, matches: map[int][]string{}}},
	{"push float",
		instr{push, 1.0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"setmatched false",
		instr{setmatched, false},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: false, pc: 0, matches: map[int][]string{}}},
	{"setmatched true",
		instr{setmatched, true},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: true, pc: 0, matches: map[int][]string{}}},
	{"otherwise",
		instr{otherwise, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fadd",
		instr{fadd, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{3.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fsub",
		instr{fsub, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{-1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmul",
		instr{fmul, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{2.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fdiv",
		instr{fdiv, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{0.5},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmod",
		instr{fmod, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fpow",
		instr{fpow, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 2.0},
		[]interface{}{4.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"getfilename",
		instr{getfilename, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{testFilename},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2s",
		instr{i2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{"1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"f2s",
		instr{f2s, nil},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3.1},
		[]interface{}{"3.1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cat",
		instr{cat, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"first", "second"},
		[]interface{}{"firstsecond"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"icmp gt false",
		instr{icmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fcmp gt false",
		instr{fcmp, 1},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"scmp eq false",
		instr{scmp, 0},
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
			obj := &object{re: tc.re, str: tc.str, m: m, prog: []instr{tc.i}}
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

			if diff := go_cmp.Diff(tc.expectedStack, v.t.stack); diff != "" {
				t.Log("unexpected vm stack state")
				t.Error(diff)
			}

			tc.expectedThread.stack = tc.expectedStack

			if diff := go_cmp.Diff(&tc.expectedThread, v.t, go_cmp.AllowUnexported(thread{})); diff != "" {
				t.Log("unexpected vm thread state")
				t.Error(diff)
				t.Errorf("\t%v", *v.t)
				t.Errorf("\t%v", tc.expectedThread)
			}

		})
	}
}

// makeVM is a helper method for construction a single-instruction VM
func makeVM(i instr, m []*metrics.Metric) *VM {
	obj := &object{m: m, prog: []instr{i}}
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
	v := makeVM(instr{inc, nil}, m)
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
	v = makeVM(instr{inc, 0}, m)
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
	v = makeVM(instr{inc, 0}, m)
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
	v = makeVM(instr{iset, nil}, m)
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
	v = makeVM(instr{iset, nil}, m)
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
	v = makeVM(instr{fset, nil}, m)
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
	v = makeVM(instr{fset, nil}, m)
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
	v = makeVM(instr{sset, nil}, m)
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
	v = makeVM(instr{dec, nil}, m)
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
	obj := &object{prog: []instr{{strptime, 0}}}
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
	obj := &object{prog: []instr{{strptime, 0}}}
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
		v := makeVM(instr{iget, nil}, m)
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
		v := makeVM(instr{fget, nil}, m)
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
		v := makeVM(instr{sget, nil}, m)
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

	v := makeVM(instr{expire, 1}, m)
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
	v := makeVM(instr{timestamp, nil}, m)
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
