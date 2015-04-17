// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net"
	"strings"

	"github.com/google/mtail/metrics"
)

const (
	collectdFormat = "PUTVAL \"%s/mtail-%s/%s-%s\" interval=%d %d:%d\n"
)

var (
	collectdExportTotal   = expvar.NewInt("collectd_export_total")
	collectdExportSuccess = expvar.NewInt("collectd_export_success")
)

// CollectdWriteMetrics writes metrics to a collectd unix socket plugin.
func (e *Exporter) CollectdWriteMetrics(socketpath string) error {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		return err
	}
	defer c.Close()

	return e.writeSocketMetrics(c, metricToCollectd, collectdExportTotal, collectdExportSuccess)
}

func metricToCollectd(m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf(collectdFormat,
		hostname,
		m.Program,
		strings.ToLower(m.Kind.String()),
		formatLabels(m.Name, l.Labels, "-", "-"),
		*pushInterval,
		l.Datum.Time.Unix(),
		l.Datum.Get())
}
