// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"context"
	"regexp"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/vm/code"
	"github.com/google/mtail/internal/vm/object"
)

var instructions = []struct {
	name          string
	i             code.Instr
	re            []*regexp.Regexp
	str           []string
	reversedStack []interface{} // stack is inverted to be pushed onto vm stack

	expectedStack  []interface{}
	expectedThread thread
}{
	{"match",
		code.Instr{code.Match, 0, 0},
		[]*regexp.Regexp{regexp.MustCompile("a*b")},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{0: {"aaaab"}}},
	},
	{"cmp lt",
		code.Instr{code.Cmp, -1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq",
		code.Instr{code.Cmp, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2", "2"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp le",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ne",
		code.Instr{code.Cmp, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp ge",
		code.Instr{code.Cmp, -1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2.0", "1.0"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0", "2"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1", "2.0"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string false",
		code.Instr{code.Cmp, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp eq string string true",
		code.Instr{code.Cmp, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "abc"},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float float",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 1.0},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt float int",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cmp gt int float",
		code.Instr{code.Cmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jnm",
		code.Instr{code.Jnm, 37, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"jm",
		code.Instr{code.Jm, 37, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{},
		thread{pc: 0, matches: map[int][]string{}}},
	{"jmp",
		code.Instr{code.Jmp, 37, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{pc: 37, matches: map[int][]string{}}},
	{"strptime",
		code.Instr{code.Strptime, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"2012/01/18 06:25:00", "2006/01/02 15:04:05"},
		[]interface{}{},
		thread{pc: 0, time: time.Date(2012, 1, 18, 6, 25, 0, 0, time.UTC),
			matches: map[int][]string{}}},
	{"iadd",
		code.Instr{code.Iadd, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"isub",
		code.Instr{code.Isub, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imul",
		code.Instr{code.Imul, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"idiv",
		code.Instr{code.Idiv, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(2)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod",
		code.Instr{code.Imod, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{4, 2},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"imod 2",
		code.Instr{code.Imod, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3, 2},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"tolower",
		code.Instr{code.Tolower, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"mIxeDCasE"},
		[]interface{}{"mixedcase"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length",
		code.Instr{code.Length, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1234"},
		[]interface{}{4},
		thread{pc: 0, matches: map[int][]string{}}},
	{"length 0",
		code.Instr{code.Length, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{""},
		[]interface{}{0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shl",
		code.Instr{code.Shl, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"shr",
		code.Instr{code.Shr, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"and",
		code.Instr{code.And, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"or",
		code.Instr{code.Or, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor",
		code.Instr{code.Xor, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 1},
		[]interface{}{int64(3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 2",
		code.Instr{code.Xor, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 3},
		[]interface{}{int64(1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"xor 3",
		code.Instr{code.Xor, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{-1, 3},
		[]interface{}{int64(^3)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"neg",
		code.Instr{code.Neg, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{0},
		[]interface{}{int64(-1)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"not",
		code.Instr{code.Not, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{false},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"pow",
		code.Instr{code.Ipow, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2, 2},
		[]interface{}{int64(4)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i pop",
		code.Instr{code.S2i, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"ff", 16},
		[]interface{}{int64(255)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2i",
		code.Instr{code.S2i, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"190"},
		[]interface{}{int64(190)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"s2f",
		code.Instr{code.S2f, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"1.0"},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2f",
		code.Instr{code.I2f, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{float64(1.0)},
		thread{pc: 0, matches: map[int][]string{}}},
	{"settime",
		code.Instr{code.Settime, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{int64(0)},
		[]interface{}{},
		thread{pc: 0, time: time.Unix(0, 0).UTC(), matches: map[int][]string{}}},
	{"push int",
		code.Instr{code.Push, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1},
		thread{pc: 0, matches: map[int][]string{}}},
	{"push float",
		code.Instr{code.Push, 1.0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"setmatched false",
		code.Instr{code.Setmatched, false, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: false, pc: 0, matches: map[int][]string{}}},
	{"setmatched true",
		code.Instr{code.Setmatched, true, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{},
		thread{matched: true, pc: 0, matches: map[int][]string{}}},
	{"otherwise",
		code.Instr{code.Otherwise, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{true},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fadd",
		code.Instr{code.Fadd, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{3.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fsub",
		code.Instr{code.Fsub, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{-1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmul",
		code.Instr{code.Fmul, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{2.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fdiv",
		code.Instr{code.Fdiv, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{0.5},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fmod",
		code.Instr{code.Fmod, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{1.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fpow",
		code.Instr{code.Fpow, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{2.0, 2.0},
		[]interface{}{4.0},
		thread{pc: 0, matches: map[int][]string{}}},
	{"getfilename",
		code.Instr{code.Getfilename, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{},
		[]interface{}{testFilename},
		thread{pc: 0, matches: map[int][]string{}}},
	{"i2s",
		code.Instr{code.I2s, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1},
		[]interface{}{"1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"f2s",
		code.Instr{code.F2s, nil, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{3.1},
		[]interface{}{"3.1"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"cat",
		code.Instr{code.Cat, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"first", "second"},
		[]interface{}{"firstsecond"},
		thread{pc: 0, matches: map[int][]string{}}},
	{"icmp gt false",
		code.Instr{code.Icmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1, 2},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"fcmp gt false",
		code.Instr{code.Fcmp, 1, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{1.0, 2.0},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
	{"scmp eq false",
		code.Instr{code.Scmp, 0, 0},
		[]*regexp.Regexp{},
		[]string{},
		[]interface{}{"abc", "def"},
		[]interface{}{false},
		thread{pc: 0, matches: map[int][]string{}}},
}

const testFilename = "test"

// Testcode.Instrs tests that each instruction behaves as expected through one
// instruction cycle.
func TestInstrs(t *testing.T) {
	for _, tc := range instructions {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			var m []*metrics.Metric
			m = append(m,
				metrics.NewMetric("foo", "test", metrics.Counter, metrics.Int),
				metrics.NewMetric("bar", "test", metrics.Counter, metrics.Int),
				metrics.NewMetric("quux", "test", metrics.Gauge, metrics.Float))
			obj := &object.Object{Regexps: tc.re, Strings: tc.str, Metrics: m, Program: []code.Instr{tc.i}}
			v := New(tc.name, obj, true, nil)
			v.t = new(thread)
			v.t.stack = make([]interface{}, 0)
			for _, item := range tc.reversedStack {
				v.t.Push(item)
			}
			v.t.matches = make(map[int][]string)
			v.input = logline.New(context.Background(), testFilename, "aaaab")
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
func makeVM(i code.Instr, m []*metrics.Metric) *VM {
	obj := &object.Object{Metrics: m, Program: []code.Instr{i}}
	v := New("test", obj, true, nil)
	v.t = new(thread)
	v.t.stack = make([]interface{}, 0)
	v.t.matches = make(map[int][]string)
	v.input = logline.New(context.Background(), testFilename, "aaaab")
	return v

}

// code.Instructions with datum store side effects
func TestDatumSetInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Gauge, metrics.String),
		metrics.NewMetric("d", "tst", metrics.Histogram, metrics.Float),
	)

	// simple inc
	v := makeVM(code.Instr{code.Inc, nil, 0}, m)
	d, err := m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "1" {
		t.Errorf("Unexpected value %v", d)
	}
	// inc by int
	v = makeVM(code.Instr{code.Inc, 0, 0}, m)
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push(2)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "3" {
		t.Errorf("Unexpected value %v", d)
	}
	// inc by str
	v = makeVM(code.Instr{code.Inc, 0, 0}, m)
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push("1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "4" {
		t.Errorf("Unexpected value %v", d)
	}
	// iset
	v = makeVM(code.Instr{code.Iset, nil, 0}, m)
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push(2)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "2" {
		t.Errorf("Unexpected value %v", d)
	}
	// iset str
	v = makeVM(code.Instr{code.Iset, nil, 0}, m)
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push("3")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "3" {
		t.Errorf("Unexpected value %v", d)
	}
	// fset
	v = makeVM(code.Instr{code.Fset, nil, 0}, m)
	d, err = m[1].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push(3.1)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "3.1" {
		t.Errorf("Unexpected value %v", d)
	}
	// fset str
	v = makeVM(code.Instr{code.Fset, nil, 0}, m)
	d, err = m[1].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push("4.1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "4.1" {
		t.Errorf("Unexpected value %v", d)
	}

	// sset
	v = makeVM(code.Instr{code.Sset, nil, 0}, m)
	d, err = m[2].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push("4.1")
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[1].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "4.1" {
		t.Errorf("Unexpected value %v", d)
	}

	// dec
	v = makeVM(code.Instr{code.Dec, nil, 0}, m)
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	datum.SetInt(d, 1, time.Now())
	v.t.Push(d)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[0].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "0" {
		t.Errorf("Unexpected value %v", d)
	}

	// set hist
	v = makeVM(code.Instr{code.Fset, nil, 0}, m)
	d, err = m[3].GetDatum()
	testutil.FatalIfErr(t, err)
	v.t.Push(d)
	v.t.Push(3.1)
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatalf("Execution failed, see info log.")
	}
	d, err = m[3].GetDatum()
	testutil.FatalIfErr(t, err)
	if d.ValueString() != "3.1" {
		t.Errorf("Unexpected value %v", d)
	}
}

func TestStrptimeWithTimezone(t *testing.T) {
	loc, _ := time.LoadLocation("Europe/Berlin")
	obj := &object.Object{Program: []code.Instr{{code.Strptime, 0, 0}}}
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
	obj := &object.Object{Program: []code.Instr{{code.Strptime, 0, 0}}}
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

// code.Instructions with datum retrieve
func TestDatumFetchInstrs(t *testing.T) {
	var m []*metrics.Metric
	m = append(m,
		metrics.NewMetric("a", "tst", metrics.Counter, metrics.Int),
		metrics.NewMetric("b", "tst", metrics.Counter, metrics.Float),
		metrics.NewMetric("c", "tst", metrics.Text, metrics.String))

	{
		// iget
		v := makeVM(code.Instr{code.Iget, nil, 0}, m)
		d, err := m[0].GetDatum()
		testutil.FatalIfErr(t, err)
		datum.SetInt(d, 37, time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, err := v.t.PopInt()
		if err != nil {
			t.Fatalf("Execution failed, see info; %v", err)
		}
		if i != 37 {
			t.Errorf("unexpected value %d", i)
		}
	}

	{
		// fget
		v := makeVM(code.Instr{code.Fget, nil, 0}, m)
		d, err := m[1].GetDatum()
		testutil.FatalIfErr(t, err)
		datum.SetFloat(d, 12.1, time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, err := v.t.PopFloat()
		if err != nil {
			t.Fatalf("Execution failed, see info: %v", err)
		}
		if i != 12.1 {
			t.Errorf("unexpected value %f", i)
		}
	}

	{
		// sget
		v := makeVM(code.Instr{code.Sget, nil, 0}, m)
		d, err := m[2].GetDatum()
		testutil.FatalIfErr(t, err)
		datum.SetString(d, "aba", time.Now())
		v.t.Push(d)
		v.execute(v.t, v.prog[0])
		if v.terminate {
			t.Fatalf("Execution failed, see info log.")
		}
		i, err := v.t.PopString()
		if err != nil {
			t.Fatalf("Execution failed, see info log: %v", err)
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

	_, err := m[0].GetDatum("z")
	testutil.FatalIfErr(t, err)

	v := makeVM(code.Instr{code.Expire, 1, 0}, m)
	v.t.Push(time.Hour)
	v.t.Push("z")
	v.t.Push(m[0])
	v.execute(v.t, v.prog[0])
	if v.terminate {
		t.Fatal("execution failed, see info log")
	}
	lv := m[0].FindLabelValueOrNil([]string{"z"})
	if lv == nil {
		t.Fatalf("couldn;t find label value in metric %#v", m[0])
	}
	if lv.Expiry != time.Hour {
		t.Fatalf("Expiry not correct, is %v", lv.Expiry)
	}
}

func TestTimestampInstr(t *testing.T) {
	var m []*metrics.Metric
	now := time.Now().UTC()
	v := makeVM(code.Instr{code.Timestamp, nil, 0}, m)
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
