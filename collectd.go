// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"expvar"
	"fmt"
	"log"
	"net"
	"strings"
)

var (
	collectd_export_total   = expvar.NewInt("collectd_export_total")
	collectd_export_success = expvar.NewInt("collectd_export_success")
)

func CollectdWriteMetrics(socketpath string) {
	c, err := net.Dial("unix", socketpath)
	if err != nil {
		log.Fatalf("Dial error: %s\n", err)
	}
	defer c.Close()

	metric_lock.RLock()
	defer metric_lock.RUnlock()
	for _, m := range metrics {
		if m.D != nil {
			collectd_export_total.Add(1)
			_, err := fmt.Fprintf(c, "PUTVAL \"%s/%s-%s/%s-%s\" interval=%d %d:%d\n",
				"localhost", // TODO(jaq): These are hacks; do this better.
				"prog",      // We don't store the name of the program that created the metric.
				m.Name,
				strings.ToLower(m.Kind.String()),
				"foo", // This is an even bigger hack; I forget why I put this here.
				push_interval,
				m.D.Time.Unix(),
				m.D.Value)
			if err == nil {
				_, err = bufio.NewReader(c).ReadString('\n')
				if err != nil {
					log.Printf("Read error: %s\n", err)
				} else {
					collectd_export_success.Add(1)
				}
			} else {
				log.Printf("Write error: %s\n", err)
			}
		} else {
			for k, d := range m.Values {
				collectd_export_total.Add(1)
				_, err := fmt.Fprintf(c, "PUTVAL \"%s/%s-%s/%s-%s\" interval=%d %d:%d\n",
					"localhost",
					"prog",
					m.Name,
					strings.ToLower(m.Kind.String()),
					strings.Join(key_unhash(k), "-"),
					push_interval,
					d.Time.Unix(),
					d.Value)
				if err == nil {
					_, err = bufio.NewReader(c).ReadString('\n')
					if err != nil {
						log.Fatalf("Read error: %s\n", err)
					} else {
						collectd_export_success.Add(1)
					}
				} else {
					log.Printf("Write error: %s\n", err)
				}
			}
		}

	}
}
