// Copyright 2015 Google Inc.  All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net/http"
	"sort"
	"strings"

	"github.com/jaqx0r/mtail/internal/metrics"
)

var exportVarzTotal = expvar.NewInt("exporter_varz_total")

const varzFormat = "%s{%s} %s\n"

// HandleVarz exports the metrics in Varz format via HTTP.
func (e *Exporter) HandleVarz(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Content-type", "text/plain")

	err := e.store.Range(func(m *metrics.Metric) error {
		select {
		case <-r.Context().Done():
			return r.Context().Err()
		default:
		}
		m.RLock()
		exportVarzTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := metricToVarz(m, l, e.omitProgLabel, e.hostname)
			fmt.Fprint(w, line)
		}
		m.RUnlock()
		return nil
	})
	if err != nil {
		http.Error(w, fmt.Sprintf("%s", err), http.StatusInternalServerError)
	}
}

func metricToVarz(m *metrics.Metric, l *metrics.LabelSet, omitProgLabel bool, hostname string) string {
	s := make([]string, 0, len(l.Labels)+2)
	for k, v := range l.Labels {
		s = append(s, fmt.Sprintf("%s=%s", k, v))
	}
	sort.Strings(s)
	if !omitProgLabel {
		s = append(s, fmt.Sprintf("prog=%s", m.Program))
	}
	s = append(s, fmt.Sprintf("instance=%s", hostname))
	return fmt.Sprintf(varzFormat,
		m.Name,
		strings.Join(s, ","),
		l.Datum.ValueString())
}
