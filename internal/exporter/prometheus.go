// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"net/http"
	"strings"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	metricExportTotal = expvar.NewInt("metric_export_total")
)

const (
	prometheusFormat = "%s{%s} %s\n"
)

func noHyphens(s string) string {
	return strings.Replace(s, "-", "_", -1)
}

// Describe implements the prometheus.Collector interface.
func (e *Exporter) Describe(c chan<- *prometheus.Desc) {
	prometheus.DescribeByCollect(e, c)
}

// Collect implements the prometheus.Collector interface.
func (e *Exporter) Collect(c chan<- prometheus.Metric) {
	e.store.RLock()
	defer e.store.RUnlock()

	for _, ml := range e.store.Metrics {
		lastSource := ""
		for _, m := range ml {
			m.RLock()
			// We don't have a way of converting text metrics to prometheus format.
			if m.Kind == metrics.Text {
				m.RUnlock()
				continue
			}
			metricExportTotal.Add(1)

			lsc := make(chan *metrics.LabelSet)
			go m.EmitLabelSets(lsc)
			for ls := range lsc {
				if lastSource == "" {
					lastSource = m.Source
				}
				var keys []string
				var vals []string
				if !e.omitProgLabel {
					keys = append(keys, "prog")
					vals = append(vals, m.Program)
				}
				for k, v := range ls.Labels {
					keys = append(keys, k)
					vals = append(vals, v)
				}
				pM, err := prometheus.NewConstMetric(
					prometheus.NewDesc(noHyphens(m.Name),
						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					promTypeForKind(m.Kind),
					promValueForDatum(ls.Datum),
					vals...)
				if err != nil {
					glog.Warning(err)
					continue
				}
				c <- prometheus.NewMetricWithTimestamp(ls.Datum.TimeUTC(), pM)
			}
			m.RUnlock()
		}
	}
}

func promTypeForKind(k metrics.Kind) prometheus.ValueType {
	switch k {
	case metrics.Counter:
		return prometheus.CounterValue
	case metrics.Gauge:
		return prometheus.GaugeValue
	case metrics.Timer:
		return prometheus.GaugeValue
	}
	return prometheus.UntypedValue
}

func promValueForDatum(d datum.Datum) float64 {
	switch n := d.(type) {
	case *datum.IntDatum:
		return float64(n.Get())
	case *datum.FloatDatum:
		return n.Get()
	}
	return 0.
}

// PrometheusHandler returns an http.Handler capable of exporting all program
// and internal metrics in the Prometheus format.
func (e *Exporter) PrometheusHandler() http.Handler {
	expvarDescs := map[string]*prometheus.Desc{
		// internal/tailer/file.go
		"log_errors_total":    prometheus.NewDesc("log_errors_total", "number of IO errors encountered per log file", []string{"logfile"}, nil),
		"log_rotations_total": prometheus.NewDesc("log_rotations_total", "number of log rotation events per log file", []string{"logfile"}, nil),
		"log_truncates_total": prometheus.NewDesc("log_truncates_total", "number of log truncation events log file", []string{"logfile"}, nil),
		"log_lines_total":     prometheus.NewDesc("log_lines_total", "number of lines read per log file", []string{"logfile"}, nil),
		// internal/vm/loader.go
		"line_count":          prometheus.NewDesc("line_count", "number of lines received by the program loader", nil, nil),
		"prog_loads_total":    prometheus.NewDesc("prog_loads_total", "number of program load events by program source filename", []string{"prog"}, nil),
		"prog_load_errors":    prometheus.NewDesc("prog_load_errors", "number of errors encountered when loading per program source filename", []string{"prog"}, nil),
		"prog_runtime_errors": prometheus.NewDesc("prog_runtime_errors", "number of errors encountered when executing programs per source filename", []string{"prog"}, nil),
		// internal/watcher/log_watcher.go
		"log_watcher_event_count": prometheus.NewDesc("log_watcher_event_count", "number of events received from fsnotify by type", []string{"type"}, nil),
		"log_watcher_error_count": prometheus.NewDesc("log_watcher_error_count", "number of errors received from fsnotify", nil, nil),
	}
	// Using a non-pedantic registry means we can be looser with metrics that
	// are not fully specified at startup.
	reg := prometheus.NewRegistry()
	reg.MustRegister(e,
		prometheus.NewGoCollector(),
		prometheus.NewProcessCollector(prometheus.ProcessCollectorOpts{}))
	prometheus.WrapRegistererWithPrefix("mtail_", reg).MustRegister(prometheus.NewExpvarCollector(expvarDescs))
	return promhttp.HandlerFor(reg, promhttp.HandlerOpts{})
}
