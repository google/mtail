// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"os"
	"reflect"
	"sort"
	"testing"
	"time"
)

func TestMetricToCollectd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}
	hostname, herr := os.Hostname()
	if herr != nil {
		t.Errorf("hostname error: %s", herr)
	}

	scalar_metric := NewMetric("foo", Counter)
	scalar_metric.GetDatum().Set(37, ts)
	r := MetricToCollectd(scalar_metric)
	expected := []string{"PUTVAL \"" + hostname + "/emtail-prog/counter-foo\" interval=60 1343124840:37\n"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	dimensioned_metric := NewMetric("bar", Gauge, "l")
	dimensioned_metric.GetDatum("quux").Set(37, ts)
	dimensioned_metric.GetDatum("snuh").Set(37, ts)
	r = MetricToCollectd(dimensioned_metric)
	sort.Strings(r)
	expected = []string{
		"PUTVAL \"" + hostname + "/emtail-prog/gauge-bar-quux\" interval=60 1343124840:37\n",
		"PUTVAL \"" + hostname + "/emtail-prog/gauge-bar-snuh\" interval=60 1343124840:37\n"}
	sort.Strings(expected)
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}
}

func TestMetricToGraphite(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalar_metric := NewMetric("foo", Counter)
	scalar_metric.GetDatum().Set(37, ts)
	r := MetricToGraphite(scalar_metric)
	sort.Strings(r)
	expected := []string{"prog.foo 37 1343124840\n"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	dimensioned_metric := NewMetric("bar", Gauge, "l")
	dimensioned_metric.GetDatum("quux").Set(37, ts)
	dimensioned_metric.GetDatum("snuh").Set(37, ts)
	r = MetricToGraphite(dimensioned_metric)
	sort.Strings(r)
	expected = []string{
		"prog.bar.quux 37 1343124840\n",
		"prog.bar.snuh 37 1343124840\n"}
	sort.Strings(expected)
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}
}

func TestMetricToStatsd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalar_metric := NewMetric("foo", Counter)
	scalar_metric.GetDatum().Set(37, ts)
	r := MetricToStatsd(scalar_metric)
	sort.Strings(r)
	expected := []string{"prog.foo:37|c"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	dimensioned_metric := NewMetric("bar", Gauge, "l")
	dimensioned_metric.GetDatum("quux").Set(37, ts)
	dimensioned_metric.GetDatum("snuh").Set(37, ts)
	r = MetricToStatsd(dimensioned_metric)
	sort.Strings(r)
	expected = []string{
		"prog.bar.quux:37|c",
		"prog.bar.snuh:42|c"}
	sort.Strings(expected)
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}
}
