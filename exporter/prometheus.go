// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net/http"
	"sort"
	"strings"

	"github.com/google/mtail/metrics"
)

var (
	metricExportTotal = expvar.NewInt("metric_export_total")
)

const (
	prometheusFormat = "%s{%s} %d\n"
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

	for _, m := range e.store.Metrics {
		m.RLock()
		metricExportTotal.Add(1)

		fmt.Fprintf(w,
			"# TYPE %s %s\n",
			noHyphens(m.Name),
			kindToPrometheusType(m.Kind))
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := metricToPrometheus(e.hostname, m, l)
			fmt.Fprint(w, line)
		}
		m.RUnlock()
	}
}

func metricToPrometheus(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	var s []string
	for k, v := range l.Labels {
		s = append(s, fmt.Sprintf("%s=%q", k, v))
	}
	sort.Strings(s)
	s = append(s, fmt.Sprintf("prog=\"%s\"", m.Program))
	s = append(s, fmt.Sprintf("instance=\"%s\"", hostname))
	return fmt.Sprintf(prometheusFormat,
		noHyphens(m.Name),
		strings.Join(s, ","),
		l.Datum.Get())
}

func kindToPrometheusType(kind metrics.Kind) string {
	if kind != metrics.Timer {
		return strings.ToLower(kind.String())
	}
	return "gauge"
}
