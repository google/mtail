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
	collectdFormat = "PUTVAL \"%s/%smtail-%s/%s-%s\" interval=%d %s:%s\n"
)

var (
	collectdSocketPath = flag.String("collectd_socketpath", "",
		"Path to collectd unixsock to write metrics to.")
	collectdPrefix = flag.String("collectd_prefix", "",
		"Prefix to use for collectd metrics.")

	collectdExportTotal   = expvar.NewInt("collectd_export_total")
	collectdExportSuccess = expvar.NewInt("collectd_export_success")
)

// metricToCollectd encodes the metric data in the collectd text protocol format.  The
// metric lock is held before entering this function.
func metricToCollectd(hostname string, m *metrics.Metric, l *metrics.LabelSet) string {
	return fmt.Sprintf(collectdFormat,
		hostname,
		*collectdPrefix,
		m.Program,
		kindToCollectdType(m.Kind),
		formatLabels(m.Name, l.Labels, "-", "-", "_"),
		*pushInterval,
		l.Datum.TimeString(),
		l.Datum.ValueString())
}

func kindToCollectdType(kind metrics.Kind) string {
	if kind != metrics.Timer {
		return strings.ToLower(kind.String())
	}
	return "gauge"
}
