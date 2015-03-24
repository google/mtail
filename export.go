// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/json"
	"expvar"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

// Commandline Flags.
var (
	collectd_socketpath *string = flag.String("collectd_socketpath", "",
		"Path to collectd unixsock to write metrics to.")
	graphite_hostport *string = flag.String("graphite_hostport", "",
		"Host:port to graphite carbon server to write metrics to.")
	statsd_hostport *string = flag.String("statsd_hostport", "",
		"Host:port to statsd server to write metrics to.")
	push_interval *int = flag.Int("metric_push_interval_seconds", 60,
		"Interval between metric pushes, in seconds")
)

var (
	// Exported variables
	collectd_export_total   = expvar.NewInt("collectd_export_total")
	collectd_export_success = expvar.NewInt("collectd_export_success")

	graphite_export_total   = expvar.NewInt("graphite_export_total")
	graphite_export_success = expvar.NewInt("graphite_export_success")

	statsd_export_total   = expvar.NewInt("statsd_export_total")
	statsd_export_success = expvar.NewInt("statsd_export_success")

	// Internal state
	hostname              string
	last_metric_push_time time.Time
)

const (
	COLLECTD_FORMAT = "PUTVAL \"%s/mtail-%s/%s-%s\" interval=%d %d:%d\n"
)

type Exporter struct {
	store metrics.Store
}

// JSON export
func (e *Exporter) handleJson(w http.ResponseWriter, r *http.Request) {
	e.store.RLock()
	defer e.store.RUnlock()

	b, err := json.MarshalIndent(e.store.Metrics, "", "  ")
	if err != nil {
		glog.Info("error marshalling metrics into json:", err.Error())
	}
	w.Write(b)
}

func (e *Exporter) CollectdWriteMetrics(socketpath string) error {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		return err
	}
	defer c.Close()

	return e.WriteSocketMetrics(c, MetricToCollectd, collectd_export_total, collectd_export_success)
}

func FormatLabels(name string, m map[string]string, ksep, sep string) string {
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

func MetricToCollectd(m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf(COLLECTD_FORMAT,
		hostname,
		m.Program,
		strings.ToLower(m.Kind.String()),
		FormatLabels(m.Name, l.Labels, "-", "-"),
		*push_interval,
		l.Datum.Time.Unix(),
		l.Datum.Get())
}

// Format a LabelSet into a string to be written to one of the timeseries sockets
type formatter func(*metrics.Metric, *metrics.LabelSet) string

func (e *Exporter) WriteSocketMetrics(c io.ReadWriter, f formatter, export_total *expvar.Int, export_success *expvar.Int) error {
	e.store.RLock()
	defer e.store.RUnlock()

	for _, m := range e.store.Metrics {
		m.RLock()
		defer m.RUnlock()
		export_total.Add(1)
		lc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lc)
		for l := range lc {
			line := f(m, l)
			_, err := fmt.Fprint(c, line)
			if err == nil {
				_, err = bufio.NewReader(c).ReadString('\n')
				if err != nil {
					return fmt.Errorf("Read error: %s\n", err)
				} else {
					export_success.Add(1)
				}
			} else {
				return fmt.Errorf("Write error: %s\n", err)
			}
		}
	}
	return nil
}

func (e *Exporter) GraphiteWriteMetrics(hostport string) error {
	c, err := net.Dial("tcp", hostport)
	if err != nil {
		return fmt.Errorf("Dial error: %s\n", err)
	}
	defer c.Close()

	return e.WriteSocketMetrics(c, MetricToGraphite, graphite_export_total, graphite_export_success)
}

func MetricToGraphite(m *metrics.Metric, l *metrics.LabelSet) string {
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s.%s %v %v\n",
		m.Program,
		FormatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get(),
		l.Datum.Time.Unix())
}

func (e *Exporter) StatsdWriteMetrics(hostport string) error {
	c, err := net.Dial("udp", hostport)
	if err != nil {
		return fmt.Errorf("Dial error: %s\n", err)
	}
	defer c.Close()
	return e.WriteSocketMetrics(c, MetricToStatsd, statsd_export_total, statsd_export_success)
}

func MetricToStatsd(m *metrics.Metric, l *metrics.LabelSet) string {
	// TODO(jaq): handle units better, send timing as |ms
	m.RLock()
	defer m.RUnlock()
	return fmt.Sprintf("%s.%s:%d|c",
		m.Program,
		FormatLabels(m.Name, l.Labels, ".", "."),
		l.Datum.Get())
}

func (e *Exporter) WriteMetrics() {
	if metrics.Metric_update_time.Sub(last_metric_push_time) <= 0 {
		return
	}
	if *collectd_socketpath != "" {
		err := e.CollectdWriteMetrics(*collectd_socketpath)
		if err != nil {
			glog.Infof("collectd write error: %s", err)
		}
	}
	if *graphite_hostport != "" {
		err := e.GraphiteWriteMetrics(*graphite_hostport)
		if err != nil {
			glog.Infof("graphite write error: %s", err)
		}
	}
	if *statsd_hostport != "" {
		err := e.StatsdWriteMetrics(*statsd_hostport)
		if err != nil {
			glog.Infof("statsd error: %s", err)
		}
	}
	last_metric_push_time = time.Now()
}

func (e *Exporter) StartMetricPush() {
	if *collectd_socketpath != "" || *graphite_hostport != "" || *statsd_hostport != "" {
		ticker := time.NewTicker(time.Duration(*push_interval) * time.Second)
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
