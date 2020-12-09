// Copyright 2019 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package vm

import (
	"bufio"
	"context"
	"expvar"
	"math"
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
				{
					Name:    "c",
					Program: "single-dash-parseint",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Hidden:  false,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{Value: 1},
						},
					},
				},
			},
		},
	},
	{"histogram",
		`histogram hist1 buckets 1, 2, 4, 8
histogram hist2 by code buckets 0, 1, 2, 4, 8
histogram hist3 by f buckets -1, 0, 1

/^(.) (\d+)/ {
  hist1 = $2
  hist2[$1] = $2
}

/^(?P<foo>[a-z]+) (?P<time>\d+)$/ {
  hist3[$foo] = $time
}
`,
		`b 3
b 3
b 3
`,
		map[string][]*metrics.Metric{
			"hist1": {
				{
					Name:    "hist1",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: 2}},
									{Range: datum.Range{Min: 2, Max: 4},
										Count: 3},
									{Range: datum.Range{Min: 4, Max: 8}},
									{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
				},
			},
			"hist2": {
				&metrics.Metric{
					Name:    "hist2",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{"code"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"b"},
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: 2}},
									{Range: datum.Range{Min: 2, Max: 4},
										Count: 3},
									{Range: datum.Range{Min: 4, Max: 8}},
									{Range: datum.Range{Min: 8, Max: math.Inf(+1)}},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: 0, Max: 1}, {Min: 1, Max: 2}, {Min: 2, Max: 4}, {Min: 4, Max: 8}, {Min: 8, Max: math.Inf(+1)}},
				},
			},
			"hist3": {
				&metrics.Metric{
					Name:    "hist3",
					Program: "histogram",
					Kind:    metrics.Histogram,
					Type:    metrics.Buckets,
					Keys:    []string{"f"},
					LabelValues: []*metrics.LabelValue{
						{
							Labels: []string{"b"},
							Value: &datum.Buckets{
								Buckets: []datum.BucketCount{
									{Range: datum.Range{Min: -1, Max: 0}},
									{Range: datum.Range{Min: 0, Max: 1}},
									{Range: datum.Range{Min: 1, Max: math.Inf(+1)},
										Count: 3},
								},
								Count: 3,
								Sum:   9,
							},
						},
					},
					Buckets: []datum.Range{{Min: -1, Max: 0}, {Min: 0, Max: 1}, {Min: 1, Max: math.Inf(+1)}},
				},
			},
		},
	},
	{"numbers",
		`counter error_log_count

/^/ +
/(?P<date>\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}) / +
/.*/ +
/$/ {
    strptime($date, "2006/01/02 15:04:05")

    error_log_count++
}
`,
		`2019/05/14 11:10:05 [warn] ...
2019/05/14 11:11:06 [warn] ...
`,
		map[string][]*metrics.Metric{
			"error_log_count": {
				{
					Name:    "error_log_count",
					Program: "numbers",
					Kind:    metrics.Counter,
					Type:    metrics.Int,
					Keys:    []string{},
					LabelValues: []*metrics.LabelValue{
						{
							Value: &datum.Int{
								Value: 2,
							},
						},
					},
				},
			},
		},
	},
}

func TestVmEndToEnd(t *testing.T) {
	if testing.Verbose() {
		defer testutil.TestSetFlag(t, "vmodule", "vm=2,loader=2,checker=2")()
	}
	for _, tc := range vmTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			w := watcher.NewFakeWatcher()
			store := metrics.NewStore()
			l, err := NewLoader("", store, w, ErrorsAbort, DumpAst, DumpAstTypes, DumpBytecode, OmitMetricSource)
			testutil.FatalIfErr(t, err)
			compileErrors := l.CompileAndRun(tc.name, strings.NewReader(tc.prog))
			testutil.FatalIfErr(t, compileErrors)
			scanner := bufio.NewScanner(strings.NewReader(tc.log))
			lineCount := 0
			for scanner.Scan() {
				lineCount++
				l.ProcessLogLine(context.Background(), logline.New(context.Background(), tc.name, scanner.Text()))
			}
			l.Close()

			// This is not good; can the loader abort on error?
			if m := expvar.Get("prog_runtime_errors_total"); m != nil {
				if val := m.(*expvar.Map).Get(tc.name); val != nil && val.String() != "0" {
					t.Errorf("Nonzero runtime errors from program: got %s", val)
				}
			}
			testutil.ExpectNoDiff(t, tc.metrics, store.Metrics, testutil.IgnoreUnexported(sync.RWMutex{}), testutil.IgnoreFields(datum.BaseDatum{}, "Time"))
		})
	}
}
