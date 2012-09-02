// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
)

func (m *Metric) String() string {
	s := fmt.Sprintf("<Metric %s %v %v", m.Name, m.Kind, m.hidden)
	if m.D == nil {
		for k, d := range m.Values {
			s += " ["
			keyvals := key_unhash(k)
			for i, key := range m.Keys {
				s += fmt.Sprintf("%s=%s ", key, keyvals[i])
			}
			s += fmt.Sprintf(" %v '%v']", d.Value, d.Time)
		}
		s += ">"
	} else {
		s += fmt.Sprintf(" %v '%v'>", m.D.Value, m.D.Time)
	}
	return s
}

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Sample log input.
	jsonfile    string // Expected metrics after processing.
}{
	{
		"examples/linecount.em",
		"testdata/linecount.log",
		"testdata/linecount.json",
	},
	{
		"examples/rsyncd.em",
		"testdata/rsyncd.log",
		"testdata/rsyncd.json",
	},
	{
		"examples/sftp.em",
		"testdata/sftp_chroot.log",
		"testdata/sftp_chroot.json",
	},
}

func CompileAndLoad(programfile string) (chan string, string) {
	metrics = make(map[string][]*Metric, 0)
	lines := make(chan string)

	p, err := os.Open(programfile)
	if err != nil {
		return lines, fmt.Sprintf("%s: could not open program file: %s", programfile, err)
	}
	defer p.Close()

	// EmtailDebug = 999 // All the debugging.

	v, errs := Compile(programfile, p)
	if errs != nil {
		return lines, fmt.Sprintf("%s: compile failed: %s", programfile, strings.Join(errs, "\n"))
	}

	vms = make([]*vm, 0)
	vms = append(vms, v)

	go RunVms(vms, lines)

	return lines, ""
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		return
	}
	for _, tc := range exampleProgramTests {
		name := filepath.Base(tc.programfile)
		if strings.HasSuffix(name, ".em") {
			name = name[:len(name)-3]
		}

		lines, errs := CompileAndLoad(tc.programfile)
		if errs != "" {
			t.Errorf(errs)
			continue
		}

		err := OneShot(tc.logfile, lines)
		if err != nil {
			t.Errorf("Oneshot failed: %s", err)
			continue
		}

		// // Dirty hack to create json files :)
		// {
		// 	j, err := os.Create(tc.jsonfile)
		// 	if err != nil {
		// 		t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
		// 		continue
		// 	}
		// 	defer j.Close()
		// 	b, err := json.MarshalIndent(metrics[name], "", "  ")
		// 	if err != nil {
		// 		t.Errorf("couldn't marshall metrics")
		// 		continue
		// 	}
		// 	j.Write(b)
		// }

		j, err := os.Open(tc.jsonfile)
		if err != nil {
			t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
			continue
		}
		defer j.Close()

		var expected_metrics []*Metric

		d := json.NewDecoder(j)
		err = d.Decode(&expected_metrics)
		if err != nil {
			t.Errorf("%s: could not decode json: %s", tc.jsonfile, err)
			continue
		}

		exported_metrics := make([]*Metric, 0)
		for _, m := range metrics[name] {
			if !m.hidden {
				exported_metrics = append(exported_metrics, m)
			}
		}

		if !reflect.DeepEqual(expected_metrics, exported_metrics) {
			t.Errorf("%s: metrics don't match.\n\texpected: %s\n\treceived: %s", tc.programfile, expected_metrics, exported_metrics)
		}
	}
}

// These benchmarks run the testdata logs through the example programs.
// Caveat emptor:
// The ns/op measure returned is the time spent on a OneShot for a single program.
// The MB/s measure should be interpreted as megalines processed per second.
func BenchmarkExamplePrograms(b *testing.B) {
	b.StopTimer()
	fmt.Println()
	for _, tc := range exampleProgramTests {
		lines, errs := CompileAndLoad(tc.programfile)
		if errs != "" {
			b.Errorf(errs)
		}
		r := testing.Benchmark(func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				b.StartTimer()
				err := OneShot(tc.logfile, lines)
				if err != nil {
					b.Errorf("Oneshot failed: %s", err)
					return
				}
				b.StopTimer()
				i, err := strconv.ParseInt(line_count.String(), 10, 64)
				if err != nil {
					b.Errorf("strconv failed: %s", err)
				}
				b.SetBytes(i)
			}
		})
		l_s := (float64(r.Bytes) * float64(r.N)) / 1e6 / r.T.Seconds()
		fmt.Printf("%s: %d runs in %s (%d ns/run, %d lines, %f Mlines/s) %s\n", tc.programfile, r.N, r.T, r.NsPerOp(), r.Bytes, l_s, r)
	}
}
