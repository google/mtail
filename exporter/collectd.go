// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"flag"
	"fmt"
	"strings"

	"github.com/google/mtail/metrics"
)

const (
	collectdFormat = "PUTVAL \"%s/mtail-%s/%s-%s\" interval=%d %d:%d\n"
)

var (
	collectdSocketPath = flag.String("collectd_socketpath", "",
		"Path to collectd unixsock to write metrics to.")

	collectdExportTotal   = expvar.NewInt("collectd_export_total")
	collectdExportSuccess = expvar.NewInt("collectd_export_success")
)

func metricToCollectd(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf(collectdFormat,
		hostname,
		m.Program,
		strings.ToLower(m.Kind.String()),
		formatLabels(m.Name, l.Labels, "-", "-"),
		*pushInterval,
		l.Datum.Time/1e9,
		l.Datum.Get())
}
