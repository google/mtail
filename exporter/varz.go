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

const varzFormat = "%s{%s} %s\n"

// HandleVarz exports the metrics in Varz format via HTTP.
func (e *Exporter) HandleVarz(w http.ResponseWriter, r *http.Request) {
	e.store.RLock()
	defer e.store.RUnlock()

	w.Header().Add("Content-type", "text/plain")

	for _, ml := range e.store.Metrics {
		for _, m := range ml {
			m.RLock()
			exportVarzTotal.Add(1)
			lc := make(chan *metrics.LabelSet)
			go m.EmitLabelSets(lc)
			for l := range lc {
				line := metricToVarz(e.o, m, l)
				fmt.Fprint(w, line)
			}
			m.RUnlock()
		}
	}
}

func metricToVarz(o Options, m *metrics.Metric, l *metrics.LabelSet) string {
	var s []string
	for k, v := range l.Labels {
		s = append(s, fmt.Sprintf("%s=%s", k, v))
	}
	sort.Strings(s)
	if !o.OmitProgLabel {
		s = append(s, fmt.Sprintf("prog=%s", m.Program))
	}
	s = append(s, fmt.Sprintf("instance=%s", o.Hostname))
	return fmt.Sprintf(varzFormat,
		m.Name,
		strings.Join(s, ","),
		l.Datum.ValueString())
}
