// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"expvar"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"strings"
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
	hostname string
)

const (
	COLLECTD_FORMAT = "PUTVAL \"%s/emtail-%s/%s-%s\" interval=%d %d:%d\n"
)

func CollectdWriteMetrics(socketpath string) error {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		return err
	}
	defer c.Close()

	return WriteSocketMetrics(c, MetricToCollectd, collectd_export_total, collectd_export_success)
}

func MetricToCollectd(prog string, m *Metric) []string {
	var ret []string
	if m.D != nil {
		s := fmt.Sprintf(COLLECTD_FORMAT,
			hostname,
			"prog", // We don't store the name of the program that created the metric.
			strings.ToLower(m.Kind.String()),
			m.Name,
			*push_interval,
			m.D.Time.Unix(),
			m.D.Value)
		ret = append(ret, s)
	} else {
		for k, d := range m.Values {
			s := fmt.Sprintf(COLLECTD_FORMAT,
				hostname,
				prog,
				strings.ToLower(m.Kind.String()),
				m.Name+"-"+strings.Join(key_unhash(k), "-"),
				*push_interval,
				d.Time.Unix(),
				d.Value)
			ret = append(ret, s)
		}
	}
	return ret
}

// Format a metric into a string to be written to one of the timeseries sockets
type formatter func(string, *Metric) []string

func WriteSocketMetrics(c io.ReadWriter, f formatter, export_total *expvar.Int, export_success *expvar.Int) error {
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	for p, _ := range metrics {
		for _, m := range metrics[p] {
			if m.hidden {
				continue
			}
			export_total.Add(1)
			for _, line := range f(p, m) {
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

func MetricToGraphite(prog string, m *Metric) []string {
	var ret []string
	if m.D != nil {
		s := fmt.Sprintf("%s.%s %v %v\n",
			prog,
			m.Name,
			m.D.Value,
			m.D.Time.Unix())
		ret = append(ret, s)
	} else {
		for k, d := range m.Values {
			s := fmt.Sprintf("%s.%s.%s %v %v\n",
				prog,
				m.Name,
				strings.Join(key_unhash(k), "."),
				d.Value,
				d.Time.Unix())
			ret = append(ret, s)
		}
	}
	return ret
}

func StatsdWriteMetrics(hostport string) error {
	c, err := net.Dial("udp", hostport)
	if err != nil {
		return fmt.Errorf("Dial error: %s\n", err)
	}
	defer c.Close()
	return WriteSocketMetrics(c, MetricToStatsd, statsd_export_total, statsd_export_success)
}

func MetricToStatsd(prog string, m *Metric) []string {
	var ret []string
	// TODO(jaq): handle units better, send timing as |ms
	if m.D != nil {
		s := fmt.Sprintf("%s.%s:%d|c",
			prog,
			m.Name,
			m.D.Value)
		ret = append(ret, s)
	} else {
		for k, d := range m.Values {
			s := fmt.Sprintf("%s.%s.%s:%d|c",
				prog,
				m.Name,
				strings.Join(key_unhash(k), "."),
				d.Value)
			ret = append(ret, s)
		}
	}
	return ret
}

func init() {
	var err error
	hostname, err = os.Hostname()
	if err != nil {
		log.Fatalf("Error getting hostname: %s\n", err)
	}
}
