// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package exporter provides the interface for getting metrics out of mtail,
// into your monitoring system of choice.
package exporter

import (
	"bytes"
	"expvar"
	"flag"
	"fmt"
	"net"
	"os"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

// Commandline Flags.
var (
	pushInterval = flag.Int("metric_push_interval_seconds", 60,
		"Interval between metric pushes, in seconds")
)

// Exporter manages the export of metrics to passive and active collectors.
type Exporter struct {
	store              *metrics.Store
	hostname           string
	lastMetricPushTime time.Time
	pushTargets        []pushOptions
}

// New creates a new Exporter.
func New(store *metrics.Store) *Exporter {
	hostname, err := os.Hostname()
	if err != nil {
		glog.Fatalf("Error getting hostname: %s\n", err)
	}
	return &Exporter{store: store, hostname: hostname}
}

// formatLabels converts a metric name and key-value map of labels to a single
// string for exporting to the correct output format for each export target.
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

// Format a LabelSet into a string to be written to one of the timeseries
// sockets.
type formatter func(string, *metrics.Metric, *metrics.LabelSet) string

func (e *Exporter) writeSocketMetrics(c net.Conn, f formatter, exportTotal *expvar.Int, exportSuccess *expvar.Int) error {
	e.store.RLock()
	defer e.store.RUnlock()

	for _, m := range e.store.Metrics {
		m.RLock()
		defer m.RUnlock()
		exportTotal.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		// This goroutine reads any bytes returned from the remote end, to keep
		// it unblocked.
		go func() {
			var buf bytes.Buffer
			for {
				_, err := buf.ReadFrom(c)
				if err == nil {
					return
				}
			}
		}()
		for l := range lc {
			line := f(e.hostname, m, l)
			n, err := fmt.Fprint(c, line)
			glog.Infof("Sent %d bytes\n", n)
			if err == nil {
				exportSuccess.Add(1)
			} else {
				return fmt.Errorf("write error: %s\n", err)
			}
		}
	}
	return nil
}

// WriteMetrics writes metrics to each of the configured services.
func (e *Exporter) WriteMetrics() {
	lastUpdateTime := metrics.MetricUpdateTime.Load().(time.Time)
	if lastUpdateTime.Sub(e.lastMetricPushTime) <= 0 {
		return
	}
	for _, target := range e.pushTargets {
		conn, err := net.Dial(target.net, target.addr)
		if err != nil {
			glog.Info("pusher dial error: %s", err)
			continue
		}
		defer conn.Close()
		err = e.writeSocketMetrics(conn, target.f, target.total, target.success)
		if err != nil {
			glog.Info("pusher write error: %s", err)
		}
	}

	// if *collectdSocketPath != "" {
	// 	err := e.CollectdWriteMetrics(*collectdSocketPath)
	// 	if err != nil {
	// 		glog.Infof("collectd write error: %s", err)
	// 	}
	// }
	// if *graphiteHostPort != "" {
	// 	err := e.GraphiteWriteMetrics(*graphiteHostPort)
	// 	if err != nil {
	// 		glog.Infof("graphite write error: %s", err)
	// 	}
	// }
	// if *statsdHostPort != "" {
	// 	err := e.StatsdWriteMetrics(*statsdHostPort)
	// 	if err != nil {
	// 		glog.Infof("statsd error: %s", err)
	// 	}
	// }
	e.lastMetricPushTime = time.Now().UTC()
}

// StartMetricPush pushes metrics to the configured services each interval.
func (e *Exporter) StartMetricPush() {
	if len(e.pushTargets) > 0 {
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

type pushOptions struct {
	net, addr      string
	f              formatter
	total, success *expvar.Int
}

// RegisterPushExport adds a push export connection to the Exporter.  Items in
// the list must describe a Dial()able connection and will have all the metrics
// pushed to each pushInterval.
func (e *Exporter) RegisterPushExport(p pushOptions) {
	e.pushTargets = append(e.pushTargets, p)
}
