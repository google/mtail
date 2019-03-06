// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"strings"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
	promtest "github.com/prometheus/client_golang/prometheus/testutil"
)

var handlePrometheusTests = []struct {
	name      string
	progLabel bool
	metrics   []*metrics.Metric
	expected  string
}{
	{"empty",
		false,
		[]*metrics.Metric{},
		"",
	},
	{"single",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}}},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{} 1
`,
	},
	{"with prog label",
		true,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}}},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{prog="test"} 1
`,
	},
	{"dimensioned",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"1", "2"}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{a="1",b="2"} 1
`,
	},
	{"gauge",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Gauge,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}}},
		},
		`# HELP foo defined at 
# TYPE foo gauge
foo{} 1
`,
	},
	{"timer",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Timer,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}}},
		},
		`# HELP foo defined at 
# TYPE foo gauge
foo{} 1
`,
	},
	{"text",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Text,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeString("hi", time.Unix(0, 0))}}},
		},
		"",
	},
	{"quotes",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"str\"bang\"blah"}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{a="str\"bang\"blah"} 1
`,
	},
	{"help",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
				Source:      "location.mtail:37",
			},
		},
		`# HELP foo defined at location.mtail:37
# TYPE foo counter
foo{} 1
`,
	},
	{"2 help with label",
		true,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test2",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
				Source:      "location.mtail:37",
			},
			{
				Name:        "foo",
				Program:     "test1",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
				Source:      "different.mtail:37",
			},
		},
		`# HELP foo defined at location.mtail:37
# TYPE foo counter
foo{prog="test2"} 1
foo{prog="test1"} 1
`,
	},
	{"histo",
		true,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Histogram,
				Keys:        []string{"a"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"bar"}, Value: datum.MakeBuckets([]datum.Range{{0, 1}, {1, 2}}, time.Unix(0, 0))}},
				Source:      "location.mtail:37",
			},
		},
		`# HELP foo defined at location.mtail:37
# TYPE foo histogram
foo_bucket{a="bar",prog="test",le="1"} 0
foo_bucket{a="bar",prog="test",le="2"} 0
foo_bucket{a="bar",prog="test",le="+Inf"} 0
foo_sum{a="bar",prog="test"} 0
foo_count{a="bar",prog="test"} 0
`,
	},
}

func TestHandlePrometheus(t *testing.T) {
	for _, tc := range handlePrometheusTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ms := metrics.NewStore()
			for _, metric := range tc.metrics {
				testutil.FatalIfErr(t, ms.Add(metric))
			}
			opts := []func(*Exporter) error{
				Hostname("gunstar"),
			}
			if !tc.progLabel {
				opts = append(opts, OmitProgLabel)
			}
			e, err := New(ms, opts...)
			testutil.FatalIfErr(t, err)
			r := strings.NewReader(tc.expected)
			if err = promtest.CollectAndCompare(e, r); err != nil {
				t.Error(err)
			}
		})
	}
}
