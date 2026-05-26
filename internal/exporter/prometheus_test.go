// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"bytes"
	"context"
	"fmt"
	"math"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
	promtest "github.com/prometheus/client_golang/prometheus/testutil"
)

var handlePrometheusTests = []struct {
	name      string
	progLabel bool
	metrics   []*metrics.Metric
	expected  string
}{
	{
		"empty",
		false,
		[]*metrics.Metric{},
		"",
	},
	{
		"single",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{} 1
`,
	},
	{
		"with prog label",
		true,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo{prog="test"} 1
`,
	},
	{
		"dimensioned",
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
	{
		"gauge",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Gauge,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo gauge
foo{} 1
`,
	},
	{
		"timer",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Timer,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo gauge
foo{} 1
`,
	},
	{
		"text",
		false,
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Text,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeString("hi", time.Unix(0, 0))}},
			},
		},
		"",
	},
	{
		"quotes",
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
	{
		"help",
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
	{
		"2 help with label",
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
	{
		"histo",
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
	{
		"histo-count-eq-inf",
		true,
		[]*metrics.Metric{
			{
				Name:    "foo",
				Program: "test",
				Kind:    metrics.Histogram,
				Keys:    []string{"a"},
				LabelValues: []*metrics.LabelValue{
					{
						Labels: []string{"bar"},
						Value: &datum.Buckets{
							Buckets: []datum.BucketCount{
								{
									Range: datum.Range{Min: 0, Max: 1},
									Count: 1,
								},
								{
									Range: datum.Range{Min: 1, Max: 2},
									Count: 1,
								},
								{
									Range: datum.Range{Min: 2, Max: math.Inf(+1)},
									Count: 2,
								},
							},
							Count: 4,
							Sum:   5,
						},
					},
				},
				Source: "location.mtail:37",
			},
		},
		`# HELP foo defined at location.mtail:37
# TYPE foo histogram
foo_bucket{a="bar",prog="test",le="1"} 1
foo_bucket{a="bar",prog="test",le="2"} 2
foo_bucket{a="bar",prog="test",le="+Inf"} 4
foo_sum{a="bar",prog="test"} 5
foo_count{a="bar",prog="test"} 4
`,
	},
}

func TestHandlePrometheus(t *testing.T) {
	for _, tc := range handlePrometheusTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			for _, metric := range tc.metrics {
				testutil.FatalIfErr(t, ms.Add(metric))
			}
			opts := []Option{
				Hostname("gunstar"),
			}
			if !tc.progLabel {
				opts = append(opts, OmitProgLabel())
			}
			e, err := New(ctx, ms, opts...)
			testutil.FatalIfErr(t, err)
			r := strings.NewReader(tc.expected)
			if err = promtest.CollectAndCompare(e, r); err != nil {
				t.Error(err)
			}
			e.Stop()
		})
	}
}

var writePrometheusTests = []struct {
	name     string
	metrics  []*metrics.Metric
	expected string
}{
	{
		"empty",
		[]*metrics.Metric{},
		"",
	},
	{
		"single",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`# HELP foo defined at 
# TYPE foo counter
foo 1
`,
	},
	{
		"multi",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
			{
				Name:        "bar",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(2, time.Unix(0, 0))}},
			},
		},
		`# HELP bar defined at 
# TYPE bar counter
bar 2
# HELP foo defined at 
# TYPE foo counter
foo 1
`,
	},
}

func TestWritePrometheus(t *testing.T) {
	for _, tc := range writePrometheusTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			for _, metric := range tc.metrics {
				testutil.FatalIfErr(t, ms.Add(metric))
			}
			opts := []Option{
				Hostname("gunstar"),
				OmitProgLabel(),
			}
			e, err := New(ctx, ms, opts...)
			testutil.FatalIfErr(t, err)

			var buf bytes.Buffer
			err = e.Write(&buf)
			testutil.FatalIfErr(t, err)
			testutil.ExpectNoDiff(t, tc.expected, buf.String())

			e.Stop()
		})
	}
}

func BenchmarkWritePrometheus(b *testing.B) {
	for _, n := range []int{100, 1000, 10000} {
		b.Run(fmt.Sprintf("labelvalues=%d", n), func(b *testing.B) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			m := metrics.NewMetric("test_metric", "test", metrics.Counter, metrics.Int, "id")
			for i := 0; i < n; i++ {
				d, err := m.GetDatum(strconv.Itoa(i))
				if err != nil {
					b.Fatal(err)
				}
				datum.SetInt(d, int64(i), time.Now())
			}
			if err := ms.Add(m); err != nil {
				b.Fatal(err)
			}

			opts := []Option{Hostname("gunstar"), OmitProgLabel()}
			e, err := New(ctx, ms, opts...)
			if err != nil {
				b.Fatal(err)
			}
			var buf bytes.Buffer
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				buf.Reset()
				if err := e.Write(&buf); err != nil {
					b.Fatal(err)
				}
			}
			e.Stop()
		})
	}
	for _, n := range []int{100, 1000, 10000} {
		b.Run(fmt.Sprintf("metrics=%d", n), func(b *testing.B) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			for i := 0; i < n; i++ {
				m := metrics.NewMetric(fmt.Sprintf("metric_%d", i), "test", metrics.Counter, metrics.Int)
				d, err := m.GetDatum()
				if err != nil {
					b.Fatal(err)
				}
				datum.SetInt(d, int64(i), time.Now())
				if err := ms.Add(m); err != nil {
					b.Fatal(err)
				}
			}

			opts := []Option{Hostname("gunstar"), OmitProgLabel()}
			e, err := New(ctx, ms, opts...)
			if err != nil {
				b.Fatal(err)
			}
			var buf bytes.Buffer
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				buf.Reset()
				if err := e.Write(&buf); err != nil {
					b.Fatal(err)
				}
			}
			e.Stop()
		})
	}
}

func TestWritePrometheusManyLabelValues(t *testing.T) {
	counts := []int{100, 1000, 10000}
	if testing.Short() {
		counts = []int{100}
	}
	for _, n := range counts {
		t.Run(fmt.Sprintf("count=%d", n), func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			m := metrics.NewMetric("test_metric", "test", metrics.Counter, metrics.Int, "id")
			for i := 0; i < n; i++ {
				d, err := m.GetDatum(strconv.Itoa(i))
				testutil.FatalIfErr(t, err)
				datum.SetInt(d, int64(i), time.Now())
			}
			testutil.FatalIfErr(t, ms.Add(m))

			opts := []Option{Hostname("gunstar"), OmitProgLabel()}
			e, err := New(ctx, ms, opts...)
			testutil.FatalIfErr(t, err)

			var buf bytes.Buffer
			err = e.Write(&buf)
			testutil.FatalIfErr(t, err)
			t.Logf("Wrote %d bytes for %d label values", buf.Len(), n)
			e.Stop()
		})
	}
}

func TestWritePrometheusManyMetrics(t *testing.T) {
	counts := []int{100, 1000, 10000}
	if testing.Short() {
		counts = []int{100}
	}
	for _, n := range counts {
		t.Run(fmt.Sprintf("count=%d", n), func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			for i := 0; i < n; i++ {
				m := metrics.NewMetric(fmt.Sprintf("metric_%d", i), "test", metrics.Counter, metrics.Int)
				d, err := m.GetDatum()
				testutil.FatalIfErr(t, err)
				datum.SetInt(d, int64(i), time.Now())
				testutil.FatalIfErr(t, ms.Add(m))
			}

			opts := []Option{Hostname("gunstar"), OmitProgLabel()}
			e, err := New(ctx, ms, opts...)
			testutil.FatalIfErr(t, err)

			var buf bytes.Buffer
			err = e.Write(&buf)
			testutil.FatalIfErr(t, err)
			t.Logf("Wrote %d bytes for %d metrics", buf.Len(), n)
			e.Stop()
		})
	}
}
