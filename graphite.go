// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

import (
	"bufio"
	"expvar"
	"fmt"
	"log"
	"net"
	"os"
	"strings"
)

var (
	graphite_export_total   = expvar.NewInt("graphite_export_total")
	graphite_export_success = expvar.NewInt("graphite_export_success")
)

func GraphiteWriteMetrics(hostport string) {
	c, err := net.Dial("tcp", hostport)
	if err != nil {
		log.Fatalf("Dial error: %s\n", err)
	}
	defer c.Close()

	metric_lock.RLock()
	defer metric_lock.RUnlock()
	for _, m := range metrics {
		if m.D != nil {
			graphite_export_total.Add(1)
			_, err := fmt.Fprintf(c, "%s %v %v\n",
				m.Name,
				m.D.Value,
				m.D.Time.Unix())
			if err == nil {
				_, err = bufio.NewReader(c).ReadString('\n')
				if err != nil {
					log.Printf("Read error: %s\n", err)
				} else {
					graphite_export_success.Add(1)
				}
			} else {
				log.Printf("Write error: %s\n", err)
			}
		} else {
			for k, d := range m.Values {
				graphite_export_total.Add(1)
				_, err := fmt.Fprintf(c, "%s.%s %v %v\n",
					m.Name,
					strings.Join(key_unhash(k), "."),
					d.Value,
					d.Time.Unix())
				if err == nil {
					_, err = bufio.NewReader(c).ReadString('\n')
					if err != nil {
						log.Fatalf("Read error: %s\n", err)
					} else {
						graphite_export_success.Add(1)
					}
				} else {
					log.Printf("Write error: %s\n", err)
				}
			}
		}

	}
}
