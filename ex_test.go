// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"reflect"
	"strings"
	"testing"
)

type exampleProgramTest struct {
	programfile string // Example program file.
	logfile     string // Test log data.
	jsonfile    string // Expected metrics after processing.
}

var exampleProgramTests = []exampleProgramTest{
	{
		"examples/rsyncd.em",
		"testdata/rsyncd.log",
		"testdata/rsyncd.json",
	},
}

func (m *Metric) String() string {
	return fmt.Sprintf("%s", *m)
}

func TestExamplePrograms(t *testing.T) {
	if testing.Short() {
		return
	}
TestLoop:
	for _, tc := range exampleProgramTests {
		metrics = make([]*Metric, 0)

		p, err := os.Open(tc.programfile)
		if err != nil {
			t.Errorf("%s: could not open program file: %s", tc.programfile, err)
			continue
		}
		defer p.Close()

		// EmtailDebug = 999 // All the debugging.

		vm, errs := Compile(tc.programfile, p)
		if errs != nil {
			t.Errorf("%s: compile failed: %s", tc.programfile, strings.Join(errs, "\n"))
			continue
		}

		l, err := os.Open(tc.logfile)
		if err != nil {
			t.Errorf("%s: could not open log file: %s", tc.logfile, err)
			continue
		}
		defer l.Close()

		r := bufio.NewReader(l)

	ReadLoop:
		for {
			line, err := r.ReadString('\n')

			switch {
			case err == io.EOF:
				break ReadLoop
			case err != nil:
				t.Errorf("%s: read error: %s", tc.programfile, err)
				continue TestLoop
			default:
				vm.Run(line)
			}
		}

		// Dirty hack to create json files :)
		// j, err := os.Create(tc.jsonfile)
		// if err != nil {
		// 	t.Errorf("%s: could not open json file: %s", tc.jsonfile, err)
		// 	continue
		// }
		// e := json.NewEncoder(j)
		// e.Encode(metrics)
		// j.Close()

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

		if !reflect.DeepEqual(expected_metrics, metrics) {
			t.Errorf("%s: metrics don't match.\n\texpected: %s\n\treceived: %s", tc.programfile, expected_metrics, metrics)
		}
	}
}
