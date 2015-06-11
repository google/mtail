// Copyright 2015 Google Inc.  All Rights Reserved.
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
	exportVarzTotal = expvar.NewInt("exporter_varz_total")
)

const varzFormat = "%s{%s} %d\n"

// HandleVarz exports the metrics in Varz format via HTTP.
func (e *Exporter) HandleVarz(w http.ResponseWriter, r *http.Request) {
	e.store.RLock()
	defer e.store.RUnlock()

	w.Header().Add("Content-type", "text/plain")

	for _, m := range e.store.Metrics {
		m.RLock()
		exportVarzTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := metricToVarz(e.hostname, m, l)
			fmt.Fprint(w, line)
		}
		m.RUnlock()
	}
}

func metricToVarz(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	var s []string
	for k, v := range l.Labels {
		s = append(s, fmt.Sprintf("%s=%s", k, v))
	}
	sort.Strings(s)
	s = append(s, fmt.Sprintf("prog=%s", m.Program))
	s = append(s, fmt.Sprintf("instance=%s", hostname))
	return fmt.Sprintf(varzFormat,
		m.Name,
		strings.Join(s, ","),
		l.Datum.Get())
}
