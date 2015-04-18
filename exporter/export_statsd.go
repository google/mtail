// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"
	"net"

	"github.com/google/mtail/metrics"
)

var (
	statsdHostPort = flag.String("statsd_hostport", "",
		"Host:port to statsd server to write metrics to.")

	statsdExportTotal   = expvar.NewInt("statsd_export_total")
	statsdExportSuccess = expvar.NewInt("statsd_export_success")
)

// StatsdWriteMetrics writes metrics to a statsd udp collector.
func (e *Exporter) StatsdWriteMetrics(hostport string) error {
	c, err := net.Dial("udp", hostport)
	if err != nil {
		return fmt.Errorf("dial error: %s\n", err)
	}
	defer c.Close()
	return e.writeSocketMetrics(c, metricToStatsd, statsdExportTotal, statsdExportSuccess)
}

func metricToStatsd(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	// TODO(jaq): handle units better, send timing as |ms
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s.%s:%d|c",
		m.Program,
		formatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get())
}
