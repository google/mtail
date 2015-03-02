package main

import (
	"expvar"
	"fmt"
	"net/http"
	"strings"
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
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	w.Header().Add("Content-type", "text/plain; version=0.0.4")

	lc := make(chan *LabelSet)
	for _, m := range metrics {
		metric_export_total.Add(1)
		fmt.Fprintf(w,
			"# TYPE %s %s\n",
			NoHyphens(m.Name),
			strings.ToLower(m.Kind.String()))
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := MetricToPrometheus(m, l)
			fmt.Fprintf(w, line)
		}
	}
}

func MetricToPrometheus(m *Metric, l *LabelSet) string {
	var s []string
	for k, v := range l.labels {
		s = append(s, fmt.Sprintf("%s=\"%s\"", k, v))
	}
	s = append(s, fmt.Sprintf("prog=\"%s\"", m.Program))
	s = append(s, fmt.Sprintf("instance=\"%s\"", hostname))
	return fmt.Sprintf(PROMETHEUS_FORMAT,
		NoHyphens(m.Name),
		strings.Join(s, ","),
		l.datum.Get(),
		l.datum.Time.UnixNano()/1e6)
}
