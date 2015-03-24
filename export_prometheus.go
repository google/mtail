package main

import (
	"expvar"
	"fmt"
	"net/http"
	"strings"

	"github.com/google/mtail/metrics"
)

var (
	metric_export_total   = expvar.NewInt("metric_export_total")
	metric_export_success = expvar.NewInt("metric_exprot_success")
)

const (
	PROMETHEUS_FORMAT = "%s{%s} %d %d\n"
)

func NoHyphens(s string) string {
	return strings.Replace(s, "-", "_", -1)
}

func handlePrometheusMetrics(w http.ResponseWriter, r *http.Request) {
	store.RLock()
	defer store.RUnlock()

	w.Header().Add("Content-type", "text/plain; version=0.0.4")

	for _, m := range store.Metrics {
		m.RLock()
		m.RUnlock()
		metric_export_total.Add(1)
		fmt.Fprintf(w,
			"# TYPE %s %s\n",
			NoHyphens(m.Name),
			strings.ToLower(m.Kind.String()))
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := MetricToPrometheus(m, l)
			fmt.Fprintf(w, line)
		}
	}
}

func MetricToPrometheus(m *metrics.Metric, l *metrics.LabelSet) string {
	var s []string
	for k, v := range l.Labels {
		s = append(s, fmt.Sprintf("%s=\"%s\"", k, v))
	}
	s = append(s, fmt.Sprintf("prog=\"%s\"", m.Program))
	s = append(s, fmt.Sprintf("instance=\"%s\"", hostname))
	return fmt.Sprintf(PROMETHEUS_FORMAT,
		NoHyphens(m.Name),
		strings.Join(s, ","),
		l.Datum.Get(),
		l.Datum.Time.UnixNano()/1e6)
}
