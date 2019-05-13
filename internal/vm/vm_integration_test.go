// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bufio"
	"expvar"
	"strings"
	"sync"
	"testing"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/watcher"
)

var vmTests = []struct {
	name    string
	prog    string
	log     string
	metrics map[string][]*metrics.Metric
}{
	{"single-dash-parseint",
		`counter c

/(?P<x>-)/ {
    $x == "-" {
        c++
    }
}
`, `123 a
- b
`,
		map[string][]*metrics.Metric{
			"c": {
				&metrics.Metric{
					Name:    "c",
					Program: "single-dash-parseint",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						&metrics.LabelValue{
							Value: &datum.IntDatum{Value: 1},
						},
					},
				},
			},
		},
	},
}

func TestVmEndToEnd(t *testing.T) {
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "vm=2,loader=2")()
	}
	for _, tc := range vmTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			w := watcher.NewFakeWatcher()
			lines := make(chan *logline.LogLine, 0)
			store := metrics.NewStore()
			l, err := NewLoader("", store, lines, w, ErrorsAbort, DumpAst, DumpAstTypes, DumpBytecode, OmitMetricSource)
			testutil.FatalIfErr(t, err)
			compileErrors := l.CompileAndRun(tc.name, strings.NewReader(tc.prog))
			testutil.FatalIfErr(t, compileErrors)
			scanner := bufio.NewScanner(strings.NewReader(tc.log))
			lineCount := 0
			for scanner.Scan() {
				lineCount++
				lines <- logline.New(tc.name, scanner.Text())
			}
			close(lines)
			<-l.VMsDone

			// This is not good; can the loader abort on error?
			if expvar.Get("prog_runtime_errors").(*expvar.Map).Get(tc.name).String() != "0" {
				t.Errorf("Nonzero runtime errors from program: got %s", expvar.Get("prog_runtime_errors").(*expvar.Map).Get(tc.name))
			}
			// t.Logf("Store is %v", store)
			if res := testutil.Diff(store.Metrics, tc.metrics, testutil.IgnoreUnexported(sync.RWMutex{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time")); res != "" {
				t.Errorf("Store didn't match:\n got %v\nwant %v\n%s", store.Metrics, tc.metrics, res)
			}
		})
	}
}
