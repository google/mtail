// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"encoding/csv"
	"encoding/json"
	"expvar"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"
	"strings"
	"time"
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
	COLLECTD_FORMAT = "PUTVAL \"%s/emtail-%s/%s-%s\" interval=%d %d:%d\n"
)

// CSV export
func handleCsv(w http.ResponseWriter, r *http.Request) {
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	c := csv.NewWriter(w)

	for prog, p := range metrics {
		for _, m := range p {
			if m.hidden {
				continue
			}
			var record []string
			record = append(record, prog)
			if m.D != nil {
				record = []string{m.Name,
					fmt.Sprintf("%d", m.Kind)}
				record = append(record, fmt.Sprintf("%d", m.D.Value))
				record = append(record, fmt.Sprintf("%s", m.D.Time))
			} else {
				record = []string{m.Name,
					fmt.Sprintf("%d", m.Kind),
					"", ""} // Datum value, timestamp
				for k, d := range m.Values {
					keyvals := key_unhash(k)
					for i, key := range m.Keys {
						record = append(record, fmt.Sprintf("%s=%s", key, keyvals[i]))
					}
					record = append(record, fmt.Sprintf("%d", d.Value))
					record = append(record, fmt.Sprintf("%s", d.Time))
				}
			}
			c.Write(record)
		}
	}

	c.Flush()
}

// JSON export
func handleJson(w http.ResponseWriter, r *http.Request) {
	metric_lock.RLock()
	defer metric_lock.RUnlock()

	exported_metrics := make(map[string][]*Metric, 0)
	for prog, _ := range metrics {
		exported_metrics[prog] = make([]*Metric, 0)
		for _, m := range metrics[prog] {
			if !m.hidden {
				exported_metrics[prog] = append(exported_metrics[prog], m)
			}
		}
	}

	b, err := json.MarshalIndent(exported_metrics, "", "  ")
	if err != nil {
		log.Println("error marshalling metrics into json:", err.Error())
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

func WriteMetrics() {
	if metric_update_time.Sub(last_metric_push_time) <= 0 {
		return
	}
	if *collectd_socketpath != "" {
		err := CollectdWriteMetrics(*collectd_socketpath)
		if err != nil {
			log.Printf("collectd write error: %s\n", err)
		}
	}
	if *graphite_hostport != "" {
		err := GraphiteWriteMetrics(*graphite_hostport)
		if err != nil {
			log.Printf("graphite write error: %s\n", err)
		}
	}
	if *statsd_hostport != "" {
		err := StatsdWriteMetrics(*statsd_hostport)
		if err != nil {
			log.Printf("statsd error: %s\n", err)
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
		log.Fatalf("Error getting hostname: %s\n", err)
	}
}
