// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net"

	"github.com/google/mtail/metrics"
)

var (
	graphiteExportTotal   = expvar.NewInt("graphite_export_total")
	graphiteExportSuccess = expvar.NewInt("graphite_export_success")
)

// GraphiteWriteMetrics writes metrics to a graphite instance.
func (e *Exporter) GraphiteWriteMetrics(hostport string) error {
	c, err := net.Dial("tcp", hostport)
	if err != nil {
		return fmt.Errorf("dial error: %s\n", err)
	}
	defer c.Close()

	return e.writeSocketMetrics(c, metricToGraphite, graphiteExportTotal, graphiteExportSuccess)
}

func metricToGraphite(m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s.%s %v %v\n",
		m.Program,
		formatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get(),
		l.Datum.Time.Unix())
}
