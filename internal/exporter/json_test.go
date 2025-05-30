// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"context"
	"io"
	"math"
	"net/http"
	"net/http/httptest"
	"sync"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

var handleJSONTests = []struct {
	name     string
	metrics  []*metrics.Metric
	expected string
}{
	{
		"empty",
		[]*metrics.Metric{},
		"[]",
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
		`[
  {
    "Name": "foo",
    "Program": "test",
    "Kind": 1,
    "Type": 0,
    "LabelValues": [
      {
        "Value": {
          "Value": 1,
          "Time": 0
        }
      }
    ]
  }
]`,
	},
	{
		"dimensioned",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"1", "2"}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
			},
		},
		`[
  {
    "Name": "foo",
    "Program": "test",
    "Kind": 1,
    "Type": 0,
    "Keys": [
      "a",
      "b"
    ],
    "LabelValues": [
      {
        "Labels": [
          "1",
          "2"
        ],
        "Value": {
          "Value": 1,
          "Time": 0
        }
      }
    ]
  }
]`,
	},
	{
		"histogram",
		[]*metrics.Metric{
			{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Histogram,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{{Labels: []string{"1", "2"}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
				Buckets:     []datum.Range{{Min: 0, Max: math.Inf(1)}},
			},
		},
		`[
  {
    "Name": "foo",
    "Program": "test",
    "Kind": 5,
    "Type": 0,
    "Keys": [
      "a",
      "b"
    ],
    "LabelValues": [
      {
        "Labels": [
          "1",
          "2"
        ],
        "Value": {
          "Value": 1,
          "Time": 0
        }
      }
    ],
    "Buckets": [
      {
        "Min": "0",
        "Max": "+Inf"
      }
    ]
  }
]`,
	},
}

func TestHandleJSON(t *testing.T) {
	for _, tc := range handleJSONTests {
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
			e.HandleJSON(response, &http.Request{})
			if response.Code != 200 {
				t.Errorf("response code not 200: %d", response.Code)
			}
			b, err := io.ReadAll(response.Body)
			if err != nil {
				t.Errorf("failed to read response: %s", err)
			}
			testutil.ExpectNoDiff(t, tc.expected, string(b), testutil.IgnoreUnexported(sync.RWMutex{}))

			e.Stop()
		})
	}
}
