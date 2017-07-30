// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/go-test/deep"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

var handleJSONTests = []struct {
	name     string
	metrics  []*metrics.Metric
	expected string
}{
	{"empty",
		[]*metrics.Metric{},
		"[]",
	},
	{"single",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
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
	{"dimensioned",
		[]*metrics.Metric{
			&metrics.Metric{
				Name:        "foo",
				Program:     "test",
				Kind:        metrics.Counter,
				Keys:        []string{"a", "b"},
				LabelValues: []*metrics.LabelValue{&metrics.LabelValue{Labels: []string{"1", "2"}, Value: datum.MakeInt(1, time.Unix(0, 0))}},
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
}

func TestHandleJSON(t *testing.T) {
	for _, tc := range handleJSONTests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			ms := metrics.NewStore()
			for _, metric := range tc.metrics {
				ms.Add(metric)
			}
			o := Options{ms, "gunstar", false}
			e, err := New(o)
			if err != nil {
				t.Fatalf("couldn't make exporter: %s", err)
			}
			response := httptest.NewRecorder()
			e.HandleJSON(response, &http.Request{})
			if response.Code != 200 {
				t.Errorf("response code not 200: %d", response.Code)
			}
			b, err := ioutil.ReadAll(response.Body)
			if err != nil {
				t.Errorf("failed to read response: %s", err)
			}
			diff := deep.Equal(tc.expected, string(b))
			if diff != nil {
				t.Error(diff)
			}
		})
	}
}
