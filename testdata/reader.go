// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testdata

import (
	"bufio"
	"io"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
	"github.com/google/mtail/metrics/datum"
)

var var_re = regexp.MustCompile(`^(counter|gauge|timer) ([^ ]+)(?: {([^}]+)})?(?: (\d+))?(?: (.+))?`)

// Find a metric in a store
func FindMetricOrNil(store *metrics.Store, name string) *metrics.Metric {
	store.RLock()
	defer store.RUnlock()
	for _, m := range store.Metrics {
		if m.Name == name {
			return m
		}
	}
	return nil
}

func ReadTestData(file io.Reader, programfile string, store *metrics.Store) {
	prog := filepath.Base(programfile)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		glog.V(2).Infof("'%s'\n", scanner.Text())
		match := var_re.FindStringSubmatch(scanner.Text())
		glog.V(2).Infof("len match: %d\n", len(match))
		if len(match) == 0 {
			continue
		}
		var keys, vals []string
		if match[3] != "" {
			for _, pair := range strings.Split(match[3], ",") {
				glog.V(2).Infof("pair: %s\n", pair)
				kv := strings.Split(pair, "=")
				keys = append(keys, kv[0])
				if kv[1] != "" {
					vals = append(vals, kv[1])
				}
			}
		}
		m := FindMetricOrNil(store, match[2])
		if m == nil {
			var kind metrics.Kind
			switch match[1] {
			case "counter":
				kind = metrics.Counter
			case "gauge":
				kind = metrics.Gauge
			case "timer":
				kind = metrics.Timer
			}
			m = metrics.NewMetric(match[2], prog, kind, metrics.Int, keys...)
			if kind == metrics.Counter && len(keys) == 0 {
				d, err := m.GetDatum()
				if err != nil {
					glog.Fatal(err)
				}
				// Initialize to zero at the zero time.
				datum.SetInt(d, 0, time.Unix(0, 0))
			}
			glog.V(2).Infof("making a new %v\n", m)
			store.Add(m)
		} else {
			glog.V(2).Infof("found %v\n", m)
		}
		if len(keys) == len(vals) {
			if match[4] != "" {
				val, err := strconv.ParseInt(match[4], 10, 64)
				if err != nil {
					glog.Fatalf("parse failed for '%s': %s", match[4], err)
				}

				var timestamp time.Time
				glog.V(2).Infof("match 5: %q\n", match[5])
				if match[5] != "" {
					timestamp, _ = time.Parse(time.RFC3339, match[5])
				}
				glog.V(2).Infof("timestamp is %s which is %v in unix\n", timestamp.Format(time.RFC3339), timestamp.Unix())

				d, err := m.GetDatum(vals...)
				if err != nil {
					glog.V(2).Infof("Failed to get datum: %s\n", err)
					continue
				}

				glog.V(2).Infof("setting %v with vals %v to %v at %v\n", d, vals, val, timestamp)
				datum.SetInt(d, val, timestamp)
			}

		}
	}
}
