// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"errors"
	"reflect"
	"sort"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
)

const prefix = "prefix"

func TestCreateExporter(t *testing.T) {
	_, err := New(nil)
	if err == nil {
		t.Error("expecting error, got nil")
	}
	store := metrics.NewStore()
	_, err = New(store)
	if err != nil {
		t.Errorf("unexpected error:%s", err)
	}
	failopt := func(*Exporter) error {
		return errors.New("busted")
	}
	_, err = New(store, failopt)
	if err == nil {
		t.Errorf("unexpected success")
	}
}

func FakeSocketWrite(f formatter, m *metrics.Metric) []string {
	// TODO(jaq): urgh looking inside m to find preallocation size
	ret := make([]string, 0, len(m.LabelValues))
	lc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lc)
	for l := range lc {
		ret = append(ret, f("gunstar", m, l))
	}
	sort.Strings(ret)
	return ret
}

func TestMetricToCollectd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}
	ms := metrics.NewStore()

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter, metrics.Int)
	d, _ := scalarMetric.GetDatum()
	datum.SetInt(d, 37, ts)
	testutil.FatalIfErr(t, ms.Add(scalarMetric))

	r := FakeSocketWrite(metricToCollectd, scalarMetric)
	expected := []string{"PUTVAL \"gunstar/mtail-prog/counter-foo\" interval=60 1343124840:37\n"}
	testutil.ExpectNoDiff(t, expected, r)

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, metrics.Int, "label")
	d, _ = dimensionedMetric.GetDatum("quux")
	datum.SetInt(d, 37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh")
	datum.SetInt(d, 37, ts)
	ms.ClearMetrics()
	testutil.FatalIfErr(t, ms.Add(dimensionedMetric))

	r = FakeSocketWrite(metricToCollectd, dimensionedMetric)
	expected = []string{
		"PUTVAL \"gunstar/mtail-prog/gauge-bar-label-quux\" interval=60 1343124840:37\n",
		"PUTVAL \"gunstar/mtail-prog/gauge-bar-label-snuh\" interval=60 1343124840:37\n"}
	testutil.ExpectNoDiff(t, expected, r)

	timingMetric := metrics.NewMetric("foo", "prog", metrics.Timer, metrics.Int)
	d, _ = timingMetric.GetDatum()
	datum.SetInt(d, 123, ts)
	testutil.FatalIfErr(t, ms.Add(timingMetric))

	r = FakeSocketWrite(metricToCollectd, timingMetric)
	expected = []string{"PUTVAL \"gunstar/mtail-prog/gauge-foo\" interval=60 1343124840:123\n"}
	testutil.ExpectNoDiff(t, expected, r)

	*collectdPrefix = prefix
	r = FakeSocketWrite(metricToCollectd, timingMetric)
	expected = []string{"PUTVAL \"gunstar/prefixmtail-prog/gauge-foo\" interval=60 1343124840:123\n"}
	testutil.ExpectNoDiff(t, expected, r)
}

func TestMetricToGraphite(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter, metrics.Int)
	d, _ := scalarMetric.GetDatum()
	datum.SetInt(d, 37, ts)
	r := FakeSocketWrite(metricToGraphite, scalarMetric)
	expected := []string{"prog.foo 37 1343124840\n"}
	testutil.ExpectNoDiff(t, expected, r)

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, metrics.Int, "host")
	d, _ = dimensionedMetric.GetDatum("quux.com")
	datum.SetInt(d, 37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh.teevee")
	datum.SetInt(d, 37, ts)
	r = FakeSocketWrite(metricToGraphite, dimensionedMetric)
	expected = []string{
		"prog.bar.host.quux_com 37 1343124840\n",
		"prog.bar.host.snuh_teevee 37 1343124840\n"}
	testutil.ExpectNoDiff(t, expected, r)

	*graphitePrefix = prefix
	r = FakeSocketWrite(metricToGraphite, dimensionedMetric)
	expected = []string{
		"prefixprog.bar.host.quux_com 37 1343124840\n",
		"prefixprog.bar.host.snuh_teevee 37 1343124840\n"}
	testutil.ExpectNoDiff(t, expected, r)
}

func TestMetricToStatsd(t *testing.T) {
	ts, terr := time.Parse("2006/01/02 15:04:05", "2012/07/24 10:14:00")
	if terr != nil {
		t.Errorf("time parse error: %s", terr)
	}

	scalarMetric := metrics.NewMetric("foo", "prog", metrics.Counter, metrics.Int)
	d, _ := scalarMetric.GetDatum()
	datum.SetInt(d, 37, ts)
	r := FakeSocketWrite(metricToStatsd, scalarMetric)
	expected := []string{"prog.foo:37|c"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	dimensionedMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, metrics.Int, "l")
	d, _ = dimensionedMetric.GetDatum("quux")
	datum.SetInt(d, 37, ts)
	d, _ = dimensionedMetric.GetDatum("snuh")
	datum.SetInt(d, 42, ts)
	r = FakeSocketWrite(metricToStatsd, dimensionedMetric)
	expected = []string{
		"prog.bar.l.quux:37|g",
		"prog.bar.l.snuh:42|g"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	timingMetric := metrics.NewMetric("foo", "prog", metrics.Timer, metrics.Int)
	d, _ = timingMetric.GetDatum()
	datum.SetInt(d, 37, ts)
	r = FakeSocketWrite(metricToStatsd, timingMetric)
	expected = []string{"prog.foo:37|ms"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	*statsdPrefix = prefix
	r = FakeSocketWrite(metricToStatsd, timingMetric)
	expected = []string{"prefixprog.foo:37|ms"}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("prefixed string didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}
}
