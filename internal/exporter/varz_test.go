// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var handleVarzTests = []struct {
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
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(1397586900, 0))}},
			},
		},
		`foo{prog=test,instance=gunstar} 1
`,
	},
	{
		"dimensioned",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"1", "2"}, Value: datum.MakeInt(1, time.Unix(1397586900, 0))}},
			},
		},
		`foo{a=1,b=2,prog=test,instance=gunstar} 1
`,
	},
	{
		"text",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Text,
				LabelValues: []*metrics.LabelValue{{Labels: []string{}, Value: datum.MakeString("hi", time.Unix(1397586900, 0))}},
			},
		},
		`foo{prog=test,instance=gunstar} hi
`,
	},
}

func TestHandleVarz(t *testing.T) {
	for _, tc := range handleVarzTests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			ms := metrics.NewStore()
			for _, metric := range tc.metrics {
				testutil.FatalIfErr(t, ms.Add(metric))
			}
			e, err := New(ctx, ms, Hostname("gunstar"))
			testutil.FatalIfErr(t, err)
			response := httptest.NewRecorder()
			e.HandleVarz(response, &http.Request{})
			if response.Code != 200 {
				t.Errorf("response code not 200: %d", response.Code)
			}
			b, err := io.ReadAll(response.Body)
			if err != nil {
				t.Errorf("failed to read response: %s", err)
			}
			testutil.ExpectNoDiff(t, tc.expected, string(b))

			e.Stop()
		})
	}
}
