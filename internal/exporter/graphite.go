// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"
	"net/http"
	"time"

	"github.com/google/mtail/internal/metrics"
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

	e.store.Range(func(m *metrics.Metric) error {
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
}

// metricToGraphite encodes a metric in the graphite text protocol format.  The
// metric lock is held before entering this function.
func metricToGraphite(hostname string, m *metrics.Metric, l *metrics.LabelSet, _ time.Duration) string {
	return fmt.Sprintf("%s%s.%s %v %v\n",
		*graphitePrefix,
		m.Program,
		formatLabels(m.Name, l.Labels, ".", ".", "_"),
		l.Datum.ValueString(),
		l.Datum.TimeString())
}
