// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

// Package exporter provides the interface for getting metrics out of mtail,
// into your monitoring system of choice.
package exporter

import (
	"errors"
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
	store       *metrics.Store
	o           Options
	pushTargets []pushOptions
}

// Options contains the required and optional parameters for constructing an
// Exporter.
type Options struct {
	Store         *metrics.Store
	Hostname      string // Not required, uses os.Hostname if zero.
	OmitProgLabel bool   // If true, don't emit the prog label that identifies the source program in variable exports.
}

// New creates a new Exporter.
func New(o Options) (*Exporter, error) {
	if o.Store == nil {
		return nil, errors.New("exporter needs a Store")
	}
	hostname := o.Hostname
	if hostname == "" {
		var err error
		hostname, err = os.Hostname()
		if err != nil {
			return nil, fmt.Errorf("Error getting hostname: %s\n", err)
		}
	}
	e := &Exporter{store: o.Store, o: o}

	if *collectdSocketPath != "" {
		o := pushOptions{"unix", *collectdSocketPath, metricToCollectd, collectdExportTotal, collectdExportSuccess}
		e.RegisterPushExport(o)
	}
	if *graphiteHostPort != "" {
		o := pushOptions{"tcp", *graphiteHostPort, metricToGraphite, graphiteExportTotal, graphiteExportSuccess}
		e.RegisterPushExport(o)
	}
	if *statsdHostPort != "" {
		o := pushOptions{"udp", *statsdHostPort, metricToStatsd, statsdExportTotal, statsdExportSuccess}
		e.RegisterPushExport(o)
	}

	return e, nil
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

	for _, ml := range e.store.Metrics {
		for _, m := range ml {
			m.RLock()
			defer m.RUnlock()
			exportTotal.Add(1)
			lc := make(chan *metrics.LabelSet)
			go m.EmitLabelSets(lc)
			for l := range lc {
				line := f(e.o.Hostname, m, l)
				n, err := fmt.Fprint(c, line)
				glog.V(2).Infof("Sent %d bytes\n", n)
				if err == nil {
					exportSuccess.Add(1)
				} else {
					return fmt.Errorf("write error: %s\n", err)
				}
			}
		}
	}
	return nil
}

// WriteMetrics writes metrics to each of the configured services.
// TODO(jaq) rename to PushMetrics.
func (e *Exporter) WriteMetrics() {
	for _, target := range e.pushTargets {
		glog.V(2).Infof("pushing to %s", target.addr)
		conn, err := net.Dial(target.net, target.addr)
		if err != nil {
			glog.Infof("pusher dial error: %s", err)
			continue
		}
		err = e.writeSocketMetrics(conn, target.f, target.total, target.success)
		if err != nil {
			glog.Infof("pusher write error: %s", err)
		}
		conn.Close()
	}
}

// StartMetricPush pushes metrics to the configured services each interval.
func (e *Exporter) StartMetricPush() {
	if len(e.pushTargets) > 0 {
		glog.Info("Started metric push.")
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
