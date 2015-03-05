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

// JSON export
func handleJson(w http.ResponseWriter, r *http.Request) {
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	b, err := json.MarshalIndent(metrics, "", "  ")
	if err != nil {
		glog.Info("error marshalling metrics into json:", err.Error())
	}
	w.Write(b)
}

func CollectdWriteMetrics(socketpath string) error {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		return err
	}
	defer c.Close()

	return WriteSocketMetrics(c, MetricToCollectd, collectd_export_total, collectd_export_success)
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

func MetricToCollectd(m *Metric, l *LabelSet) string {
	return fmt.Sprintf(COLLECTD_FORMAT,
		hostname,
		m.Program,
		strings.ToLower(m.Kind.String()),
		FormatLabels(m.Name, l.labels, "-", "-"),
		*push_interval,
		l.datum.Time.Unix(),
		l.datum.Get())
}

// Format a LabelSet into a string to be written to one of the timeseries sockets
type formatter func(*Metric, *LabelSet) string

func WriteSocketMetrics(c io.ReadWriter, f formatter, export_total *expvar.Int, export_success *expvar.Int) error {
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	for _, m := range metrics {
		export_total.Add(1)
		lc := make(chan *LabelSet)
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

func GraphiteWriteMetrics(hostport string) error {
	c, err := net.Dial("tcp", hostport)
	if err != nil {
		return fmt.Errorf("Dial error: %s\n", err)
	}
	defer c.Close()

	return WriteSocketMetrics(c, MetricToGraphite, graphite_export_total, graphite_export_success)
}

func MetricToGraphite(m *Metric, l *LabelSet) string {
	return fmt.Sprintf("%s.%s %v %v\n",
		m.Program,
		FormatLabels(m.Name, l.labels, ".", "."),
		l.datum.Get(),
		l.datum.Time.Unix())
}

func StatsdWriteMetrics(hostport string) error {
	c, err := net.Dial("udp", hostport)
	if err != nil {
		return fmt.Errorf("Dial error: %s\n", err)
	}
	defer c.Close()
	return WriteSocketMetrics(c, MetricToStatsd, statsd_export_total, statsd_export_success)
}

func MetricToStatsd(m *Metric, l *LabelSet) string {
	// TODO(jaq): handle units better, send timing as |ms
	return fmt.Sprintf("%s.%s:%d|c",
		m.Program,
		FormatLabels(m.Name, l.labels, ".", "."),
		l.datum.Get())
}

func WriteMetrics() {
	if metric_update_time.Sub(last_metric_push_time) <= 0 {
		return
	}
	if *collectd_socketpath != "" {
		err := CollectdWriteMetrics(*collectd_socketpath)
		if err != nil {
			glog.Infof("collectd write error: %s", err)
		}
	}
	if *graphite_hostport != "" {
		err := GraphiteWriteMetrics(*graphite_hostport)
		if err != nil {
			glog.Infof("graphite write error: %s", err)
		}
	}
	if *statsd_hostport != "" {
		err := StatsdWriteMetrics(*statsd_hostport)
		if err != nil {
			glog.Infof("statsd error: %s", err)
		}
	}
	last_metric_push_time = time.Now()
}

func StartMetricPush() {
	if *collectd_socketpath != "" || *graphite_hostport != "" || *statsd_hostport != "" {
		ticker := time.NewTicker(time.Duration(*push_interval) * time.Second)
		go func() {
			for {
				select {
				case <-ticker.C:
					WriteMetrics()
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
