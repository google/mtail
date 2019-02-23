// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"

	"github.com/prometheus/client_golang/prometheus"
)

var (
	metricExportTotal = expvar.NewInt("metric_export_total")
)

func noHyphens(s string) string {
	return strings.Replace(s, "-", "_", -1)
}

// Describe implements the prometheus.Collector interface.
func (e *Exporter) Describe(c chan<- *prometheus.Desc) {
	prometheus.DescribeByCollect(e, c)
}

// Collect implements the prometheus.Collector interface.
func (e *Exporter) Collect(c chan<- prometheus.Metric) {
	e.store.RLock()
	defer e.store.RUnlock()

	for _, ml := range e.store.Metrics {
		lastSource := ""
		for _, m := range ml {
			m.RLock()
			// We don't have a way of converting text metrics to prometheus format.
			if m.Kind == metrics.Text {
				m.RUnlock()
				continue
			}
			metricExportTotal.Add(1)

			lsc := make(chan *metrics.LabelSet)
			go m.EmitLabelSets(lsc)
			for ls := range lsc {
				if lastSource == "" {
					lastSource = m.Source
				}
				var keys []string
				var vals []string
				if !e.omitProgLabel {
					keys = append(keys, "prog")
					vals = append(vals, m.Program)
				}
				for k, v := range ls.Labels {
					keys = append(keys, k)
					vals = append(vals, v)
				}
				pM, err := prometheus.NewConstMetric(
					prometheus.NewDesc(noHyphens(m.Name),
						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					promTypeForKind(m.Kind),
					promValueForDatum(ls.Datum),
					vals...)
				if err != nil {
					glog.Warning(err)
					continue
				}
				c <- prometheus.NewMetricWithTimestamp(ls.Datum.TimeUTC(), pM)
			}
			m.RUnlock()
		}
	}
}

func promTypeForKind(k metrics.Kind) prometheus.ValueType {
	switch k {
	case metrics.Counter:
		return prometheus.CounterValue
	case metrics.Gauge:
		return prometheus.GaugeValue
	case metrics.Timer:
		return prometheus.GaugeValue
	}
	return prometheus.UntypedValue
}

func promValueForDatum(d datum.Datum) float64 {
	switch n := d.(type) {
	case *datum.IntDatum:
		return float64(n.Get())
	case *datum.FloatDatum:
		return n.Get()
	}
	return 0.
}
