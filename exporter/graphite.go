// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"

	"github.com/google/mtail/metrics"
)

var (
	graphiteHostPort = flag.String("graphite_host_port", "",
		"Host:port to graphite carbon server to write metrics to.")
	graphitePrefix = flag.String("graphite_prefix", "",
		"Prefix to use for graphite metrics.")

	graphiteExportTotal   = expvar.NewInt("graphite_export_total")
	graphiteExportSuccess = expvar.NewInt("graphite_export_success")
)

func metricToGraphite(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s%s.%s %v %v\n",
		*graphitePrefix,
		m.Program,
		formatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get(),
		l.Datum.Time/1e9)
}
