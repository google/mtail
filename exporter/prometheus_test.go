// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

var handlePrometheusTests = []struct {
	name     string
	metrics  []*metrics.Metric
	expected string
}{
	{"empty",
		[]*metrics.Metric{},
		"",
	},
	{"single",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{}, Value: &metrics.Datum{Value: 1}}}},
		},
		`# TYPE foo counter
foo{prog="test",instance="gunstar"} 1
`,
	},
	{"dimensioned",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{"1", "2"}, Value: &metrics.Datum{Value: 1}}},
			},
		},
		`# TYPE foo counter
foo{a="1",b="2",prog="test",instance="gunstar"} 1
`,
	},
	{"gauge",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Gauge,
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{}, Value: &metrics.Datum{Value: 1}}}},
		},
		`# TYPE foo gauge
foo{prog="test",instance="gunstar"} 1
`,
	},
	{"timer",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Timer,
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{}, Value: &metrics.Datum{Value: 1}}}},
		},
		`# TYPE foo gauge
foo{prog="test",instance="gunstar"} 1
`,
	},
	{"quotes",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a"},
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{"str\"bang\"blah"}, Value: &metrics.Datum{Value: 1}}},
			},
		},
		`# TYPE foo counter
foo{a="str\"bang\"blah",prog="test",instance="gunstar"} 1
`,
	},
}

func TestHandlePrometheus(t *testing.T) {
	for _, tc := range handlePrometheusTests {
		ms := metrics.NewStore()
		for _, metric := range tc.metrics {
			ms.Add(metric)
		}
		o := Options{ms, "gunstar"}
		e, err := New(o)
		if err != nil {
			t.Fatalf("couldn't make exporter: %s", err)
		}
		response := httptest.NewRecorder()
		e.HandlePrometheusMetrics(response, &http.Request{})
		if response.Code != 200 {
			t.Errorf("test case %s: response code not 200: %d", tc.name, response.Code)
		}
		b, err := ioutil.ReadAll(response.Body)
		if err != nil {
			t.Errorf("test case %s: failed to read response: %s", tc.name, err)
		}
		diff := pretty.Compare(string(b), tc.expected)
		if len(diff) > 0 {
			t.Errorf("test case %s: response not expected:\n%s", tc.name, diff)
		}
	}
}
