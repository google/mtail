// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"context"
	"errors"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"github.com/jaqx0r/mtail/internal/testutil"
)

const prefix = "prefix"

func TestCreateExporter(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	store := metrics.NewStore()

	e, err := New(ctx, store)
	if err != nil {
		t.Errorf("New(ctx, store) unexpected error: %v", err)
	}
	e.Stop()

	failopt := func(*Exporter) error {
		return errors.New("busted") // nolint:goerr113
	}
	_, err = New(ctx, store, failopt)
	if err == nil {
		t.Error("New(ctx, store, fail) -> unexpected success")
	}

	_, err = New(ctx, nil)
	if err == nil {
		t.Error("New(ctx, nil) -> nil, expecting error")
	}
}

func FakeSocketWrite(f formatter, m *metrics.Metric) []string {
	ret := make([]string, 0)
	lc := make(chan *metrics.LabelSet)
	d := 60 * time.Second
	go m.EmitLabelSets(lc)
	for l := range lc {
		ret = append(ret, f("gunstar", m, l, d))
	}
	sort.Strings(ret)
	return ret
}

func TestMetricToCollectd(t *testing.T) {
	*collectdPrefix = ""
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
		"PUTVAL \"gunstar/mtail-prog/gauge-bar-label-snuh\" interval=60 1343124840:37\n",
	}
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
	*graphitePrefix = ""
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
		"prog.bar.host.snuh_teevee 37 1343124840\n",
	}
	testutil.ExpectNoDiff(t, expected, r)

	histogramMetric := metrics.NewMetric("hist", "prog", metrics.Histogram, metrics.Buckets, "xxx")
	lv := &metrics.LabelValue{Labels: []string{"bar"}, Value: datum.MakeBuckets([]datum.Range{{0, 10}, {10, 20}}, time.Unix(0, 0))}
	histogramMetric.AppendLabelValue(lv)
	d, _ = histogramMetric.GetDatum("bar")
	datum.SetFloat(d, 1, ts)
	datum.SetFloat(d, 5, ts)
	datum.SetFloat(d, 15, ts)
	datum.SetFloat(d, 12, ts)
	datum.SetFloat(d, 19, ts)
	datum.SetFloat(d, 1000, ts)
	r = FakeSocketWrite(metricToGraphite, histogramMetric)
	r = strings.Split(strings.TrimSuffix(r[0], "\n"), "\n")
	sort.Strings(r)
	expected = []string{
		"prog.hist.xxx.bar 1052 1343124840",
		"prog.hist.xxx.bar.bin_10 2 1343124840",
		"prog.hist.xxx.bar.bin_20 3 1343124840",
		"prog.hist.xxx.bar.bin_inf 1 1343124840",
		"prog.hist.xxx.bar.count 6 1343124840",
	}
	testutil.ExpectNoDiff(t, expected, r)

	*graphitePrefix = prefix
	r = FakeSocketWrite(metricToGraphite, dimensionedMetric)
	expected = []string{
		"prefixprog.bar.host.quux_com 37 1343124840\n",
		"prefixprog.bar.host.snuh_teevee 37 1343124840\n",
	}
	testutil.ExpectNoDiff(t, expected, r)
}

func TestMetricToStatsd(t *testing.T) {
	*statsdPrefix = ""
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
		"prog.bar.l.snuh:42|g",
	}
	if !reflect.DeepEqual(expected, r) {
		t.Errorf("String didn't match:\n\texpected: %v\n\treceived: %v", expected, r)
	}

	multiLabelMetric := metrics.NewMetric("bar", "prog", metrics.Gauge, metrics.Int, "c", "a", "b")
	d, _ = multiLabelMetric.GetDatum("x", "z", "y")
	datum.SetInt(d, 37, ts)
	r = FakeSocketWrite(metricToStatsd, multiLabelMetric)
	expected = []string{"prog.bar.a.z.b.y.c.x:37|g"}
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
