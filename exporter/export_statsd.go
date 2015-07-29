// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"

	"github.com/google/mtail/metrics"
)

var (
	statsdHostPort = flag.String("statsd_hostport", "",
		"Host:port to statsd server to write metrics to.")

	statsdExportTotal   = expvar.NewInt("statsd_export_total")
	statsdExportSuccess = expvar.NewInt("statsd_export_success")
)

func metricToStatsd(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	var t string
	switch m.Kind {
	case metrics.Counter:
		t = "c" // StatsD Counter
	case metrics.Gauge:
		t = "g" // StatsD Gauge
	case metrics.Timer:
		t = "ms" // StatsD Timer
	}
	return fmt.Sprintf("%s.%s:%d|%s",
		m.Program,
		formatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get(), t)
}
