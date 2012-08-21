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

var exampleProgramTests = []struct {
	programfile string // Example program file.
	logfile     string // Test log data.
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
TestLoop:
	for _, tc := range exampleProgramTests {
		lines, errs := CompileAndLoad(tc.programfile)
		if errs != "" {
			t.Errorf(errs)
			continue TestLoop
		}

		err := OneShot(tc.logfile, lines)
		if err != nil {
			t.Errorf("Oneshot failed: %s", err)
			continue TestLoop
		}

		// // Dirty hack to create json files :)
		// {
		// 	j, err := os.Create(tc.jsonfile)
		// 	if err != nil {
		// 		t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
		// 		continue
		// 	}
		// 	defer j.Close()
		// 	b, err := json.MarshalIndent(metrics, "", "  ")
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
		name := filepath.Base(tc.programfile)
		if strings.HasSuffix(name, ".em") {
			name = name[:len(name)-3]
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

func runProgramBenchmark(programfile string, logfile string, b *testing.B) {
	b.StopTimer()
	lines, errs := CompileAndLoad(programfile)
	if errs != "" {
		b.Errorf(errs)
		return
	}
	for run := 0; run < b.N; run++ {
		b.StartTimer()
		err := OneShot(logfile, lines)
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
}

// These benchmarks run the testdata logs through the example programs.
// Caveat emptor:
// The ns/op measure returned is the time spent on a OneShot for a single program.
// The MB/s measure should be interpreted as megalines processed per second.

func BenchmarkExampleLineCount(b *testing.B) {
	runProgramBenchmark("examples/linecount.em", "testdata/linecount.log", b)
}

func BenchmarkExampleRsyncd(b *testing.B) {
	runProgramBenchmark("examples/rsyncd.em", "testdata/rsyncd.log", b)
}
