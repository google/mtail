// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net/http"
	"sort"
	"strconv"
	"strings"

	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

var (
	metricExportTotal = expvar.NewInt("metric_export_total")
)

const (
	prometheusFormat = "%s{%s} %s\n"
)

func noHyphens(s string) string {
	return strings.Replace(s, "-", "_", -1)
}

// HandlePrometheusMetrics exports the metrics in a format readable by
// Prometheus via HTTP.
func (e *Exporter) HandlePrometheusMetrics(w http.ResponseWriter, r *http.Request) {
	e.store.RLock()
	defer e.store.RUnlock()

	w.Header().Add("Content-type", "text/plain; version=0.0.4")

	for _, ml := range e.store.Metrics {
		emittype := true
		for _, m := range ml {
			m.RLock()
			metricExportTotal.Add(1)

			if emittype {
				fmt.Fprintf(w,
					"# TYPE %s %s\n",
					noHyphens(m.Name),
					kindToPrometheusType(m.Kind))
				emittype = false
			}

			lc := make(chan *metrics.LabelSet)
			go m.EmitLabelSets(lc)
			for l := range lc {
				if m.Source != "" {
					fmt.Fprintf(w, "# %s defined at %s\n", noHyphens(m.Name), m.Source)
				}
				line := metricToPrometheus(m, l, e.omitProgLabel)
				fmt.Fprint(w, line)
			}
			m.RUnlock()
		}
	}
}

func prometheusMetricLine(name, value string, labels ...string) string {
	return fmt.Sprintf(prometheusFormat, noHyphens(name), strings.Join(labels, ","), value)
}

func metricToPrometheus(m *metrics.Metric, l *metrics.LabelSet, omitProgLabel bool) string {
	var s []string
	for k, v := range l.Labels {
		// Prometheus quotes the value of each label=value pair.
		s = append(s, fmt.Sprintf("%s=%q", k, v))
	}
	sort.Strings(s)
	if !omitProgLabel {
		s = append(s, fmt.Sprintf("prog=\"%s\"", m.Program))
	}
	switch m.Kind {
	case metrics.Histogram:
		lines := []string{}

		b, ok := l.Datum.(*datum.BucketsDatum)
		if !ok {
			return ""
		}

		lines = append(lines, prometheusMetricLine(
			m.Name+"_count",
			strconv.FormatUint(b.Count(), 10),
			s...,
		))

		lines = append(lines, prometheusMetricLine(
			m.Name+"_sum",
			strconv.FormatFloat(b.Sum(), 'g', -1, 64),
			s...,
		))

		for r, c := range b.Buckets() {
			lines = append(lines, prometheusMetricLine(
				m.Name+"_bucket",
				strconv.FormatUint(c, 10),
				append(s, fmt.Sprintf("le=\"%f\"", r.Max))...,
			))
		}

		return strings.Join(lines, "")
	default:
		return prometheusMetricLine(m.Name, l.Datum.Value(), s...)
	}
}

func kindToPrometheusType(kind metrics.Kind) string {
	if kind != metrics.Timer {
		return strings.ToLower(kind.String())
	}
	return "gauge"
}
