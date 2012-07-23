// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"expvar"
	"fmt"
	"log"
	"net"
	"strings"
)

var (
	graphite_export_total   = expvar.NewInt("graphite_export_total")
	graphite_export_success = expvar.NewInt("graphite_export_success")
)

func GraphiteWriteMetrics(hostport string) error {
	c, err := net.Dial("tcp", hostport)
	if err != nil {
		log.Printf("Dial error: %s\n", err)
		return err
	}
	defer c.Close()

	return WriteSocketMetrics(c, MetricToGraphite, graphite_export_total, graphite_export_success)
}

func MetricToGraphite(m *Metric) []string {
	var ret []string
	if m.D != nil {
		s := fmt.Sprintf("%s %v %v\n",
			m.Name,
			m.D.Value,
			m.D.Time.Unix())
		ret = append(ret, s)
	} else {
		for k, d := range m.Values {
			s := fmt.Sprintf("%s.%s %v %v\n",
				m.Name,
				strings.Join(key_unhash(k), "."),
				d.Value,
				d.Time.Unix())
			ret = append(ret, s)
		}
	}
	return ret
}
