// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"encoding/json"
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

		v, errs := Compile(tc.programfile, p)
		if errs != nil {
			t.Errorf("%s: compile failed: %s", tc.programfile, strings.Join(errs, "\n"))
			continue
		}

		vms = make([]*vm, 0)
		vms = append(vms, v)

		lines := make(chan string)
		go RunVms(vms, lines)

		err = OneShot(tc.logfile, lines)
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

		exported_metrics := make([]*Metric, 0)
		for _, m := range metrics {
			if !m.hidden {
				exported_metrics = append(exported_metrics, m)
			}
		}

		if !reflect.DeepEqual(expected_metrics, exported_metrics) {
			t.Errorf("%s: metrics don't match.\n\texpected: %s\n\treceived: %s", tc.programfile, expected_metrics, exported_metrics)
		}
	}
}
