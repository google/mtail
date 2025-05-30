// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"
	"math"
	"net/http"
	"strings"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
)

var (
	graphiteHostPort = flag.String("graphite_host_port", "",
		"Host:port to graphite carbon server to write metrics to.")
	graphitePrefix = flag.String("graphite_prefix", "",
		"Prefix to use for graphite metrics.")

	graphiteExportTotal   = expvar.NewInt("graphite_export_total")
	graphiteExportSuccess = expvar.NewInt("graphite_export_success")
)

func (e *Exporter) HandleGraphite(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Content-type", "text/plain")

	err := e.store.Range(func(m *metrics.Metric) error {
		select {
		case <-r.Context().Done():
			return r.Context().Err()
		default:
		}
		m.RLock()
		graphiteExportTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := metricToGraphite(e.hostname, m, l, 0)
			fmt.Fprint(w, line)
		}
		m.RUnlock()
		return nil
	})
	if err != nil {
		http.Error(w, fmt.Sprintf("%s", err), http.StatusInternalServerError)
	}
}

// metricToGraphite encodes a metric in the graphite text protocol format.  The
// metric lock is held before entering this function.
func metricToGraphite(_ string, m *metrics.Metric, l *metrics.LabelSet, _ time.Duration) string {
	var b strings.Builder
	if m.Kind == metrics.Histogram && m.Type == metrics.Buckets {
		d := m.LabelValues[0].Value
		buckets := datum.GetBuckets(d)
		for r, c := range buckets.GetBuckets() {
			var binName string
			if math.IsInf(r.Max, 1) {
				binName = "inf"
			} else {
				binName = fmt.Sprintf("%v", r.Max)
			}
			fmt.Fprintf(&b, "%s%s.%s.bin_%s %v %v\n",
				*graphitePrefix,
				m.Program,
				formatLabels(m.Name, l.Labels, ".", ".", "_"),
				binName,
				c,
				l.Datum.TimeString())
		}
		fmt.Fprintf(&b, "%s%s.%s.count %v %v\n",
			*graphitePrefix,
			m.Program,
			formatLabels(m.Name, l.Labels, ".", ".", "_"),
			buckets.GetCount(),
			l.Datum.TimeString())
	}
	fmt.Fprintf(&b, "%s%s.%s %v %v\n",
		*graphitePrefix,
		m.Program,
		formatLabels(m.Name, l.Labels, ".", ".", "_"),
		l.Datum.ValueString(),
		l.Datum.TimeString())
	return b.String()
}
