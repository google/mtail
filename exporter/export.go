// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package exporter provides the interface for getting metrics out of mtail,
// into your monitoring system of choice.
package exporter

import (
	"bufio"
	"expvar"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

// Commandline Flags.
var (
	collectdSocketPath = flag.String("collectd_socketpath", "",
		"Path to collectd unixsock to write metrics to.")
	graphiteHostPort = flag.String("graphite_host_port", "",
		"Host:port to graphite carbon server to write metrics to.")
	statsdHostPort = flag.String("statsd_hostport", "",
		"Host:port to statsd server to write metrics to.")
	pushInterval = flag.Int("metric_push_interval_seconds", 60,
		"Interval between metric pushes, in seconds")
)

var (
	// Exported variables
	collectdExportTotal   = expvar.NewInt("collectd_export_total")
	collectdExportSuccess = expvar.NewInt("collectd_export_success")

	graphiteExportTotal   = expvar.NewInt("graphite_export_total")
	graphiteExportSuccess = expvar.NewInt("graphite_export_success")

	statsdExportTotal   = expvar.NewInt("statsd_export_total")
	statsdExportSuccess = expvar.NewInt("statsd_export_success")

	// Internal state
	hostname           string
	lastMetricPushTime time.Time
)

const (
	collectdFormat = "PUTVAL \"%s/mtail-%s/%s-%s\" interval=%d %d:%d\n"
)

// Exporter manages the export of metrics to passive and active collectors.
type Exporter struct {
	store *metrics.Store
}

// New creates a new Exporter.
func New(store *metrics.Store) *Exporter {
	return &Exporter{store}
}

// CollectdWriteMetrics writes metrics to a collectd unix socket plugin.
func (e *Exporter) CollectdWriteMetrics(socketpath string) error {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		return err
	}
	defer c.Close()

	return e.writeSocketMetrics(c, metricToCollectd, collectdExportTotal, collectdExportSuccess)
}

func formatLabels(name string, m map[string]string, ksep, sep string) string {
	r := name
	if len(m) > 0 {
		var s []string
		for k, v := range m {
			s = append(s, fmt.Sprintf("%s%s%s", k, ksep, v))
		}
		return r + sep + strings.Join(s, sep)
	}
	return r
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

// Format a LabelSet into a string to be written to one of the timeseries sockets
type formatter func(*metrics.Metric, *metrics.LabelSet) string

func (e *Exporter) writeSocketMetrics(c io.ReadWriter, f formatter, exportTotal *expvar.Int, exportSuccess *expvar.Int) error {
	e.store.RLock()
	defer e.store.RUnlock()

	for _, m := range e.store.Metrics {
		m.RLock()
		defer m.RUnlock()
		exportTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := f(m, l)
			_, err := fmt.Fprint(c, line)
			if err == nil {
				_, err = bufio.NewReader(c).ReadString('\n')
				if err != nil {
					return fmt.Errorf("read error: %s\n", err)
				}
				exportSuccess.Add(1)
			} else {
				return fmt.Errorf("write error: %s\n", err)
			}
		}
	}
	return nil
}

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

// StatsdWriteMetrics writes metrics to a statsd udp collector.
func (e *Exporter) StatsdWriteMetrics(hostport string) error {
	c, err := net.Dial("udp", hostport)
	if err != nil {
		return fmt.Errorf("dial error: %s\n", err)
	}
	defer c.Close()
	return e.writeSocketMetrics(c, metricToStatsd, statsdExportTotal, statsdExportSuccess)
}

func metricToStatsd(m *metrics.Metric, l *metrics.LabelSet) string {
	// TODO(jaq): handle units better, send timing as |ms
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s.%s:%d|c",
		m.Program,
		formatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get())
}

// WriteMetrics writes metrics to each of the configured services.
func (e *Exporter) WriteMetrics() {
	lastUpdateTime := metrics.MetricUpdateTime.Load().(time.Time)
	if lastUpdateTime.Sub(lastMetricPushTime) <= 0 {
		return
	}
	if *collectdSocketPath != "" {
		err := e.CollectdWriteMetrics(*collectdSocketPath)
		if err != nil {
			glog.Infof("collectd write error: %s", err)
		}
	}
	if *graphiteHostPort != "" {
		err := e.GraphiteWriteMetrics(*graphiteHostPort)
		if err != nil {
			glog.Infof("graphite write error: %s", err)
		}
	}
	if *statsdHostPort != "" {
		err := e.StatsdWriteMetrics(*statsdHostPort)
		if err != nil {
			glog.Infof("statsd error: %s", err)
		}
	}
	lastMetricPushTime = time.Now().UTC()
}

// StartMetricPush pushes metrics to the configured services each interval.
func (e *Exporter) StartMetricPush() {
	if *collectdSocketPath != "" || *graphiteHostPort != "" || *statsdHostPort != "" {
		ticker := time.NewTicker(time.Duration(*pushInterval) * time.Second)
		go func() {
			for {
				select {
				case <-ticker.C:
					e.WriteMetrics()
				}
			}
		}()
	}
}

func init() {
	var err error
	hostname, err = os.Hostname()
	if err != nil {
		glog.Fatalf("Error getting hostname: %s\n", err)
	}
}
