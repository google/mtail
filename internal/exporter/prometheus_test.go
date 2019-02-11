// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
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
		`# TYPE foo counter
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
		`# TYPE foo counter
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
		`# TYPE foo counter
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
		`# TYPE foo gauge
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
		`# TYPE foo gauge
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
		`# TYPE foo counter
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
		`# TYPE foo counter
# foo defined at location.mtail:37
foo{} 1
`,
	},
	{"2 help",
		false,
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
		`# TYPE foo counter
# foo defined at location.mtail:37
foo{} 1
# foo defined at different.mtail:37
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
		`# TYPE foo counter
# foo defined at location.mtail:37
foo{prog="test2"} 1
# foo defined at different.mtail:37
foo{prog="test1"} 1
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
			if err != nil {
				t.Fatalf("couldn't make exporter: %s", err)
			}
			response := httptest.NewRecorder()
			e.HandlePrometheusMetrics(response, &http.Request{})
			if response.Code != 200 {
				t.Errorf("response code not 200: %d", response.Code)
			}
			b, err := ioutil.ReadAll(response.Body)
			if err != nil {
				t.Errorf(" failed to read response: %s", err)
			}
			diff := testutil.Diff(tc.expected, string(b))
			if diff != "" {
				t.Error(diff)
			}
		})
	}
}
