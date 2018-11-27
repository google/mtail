// Copyright 2016 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package testutil

import (
	"bufio"
	"io"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
)

var varRe = regexp.MustCompile(`^(counter|gauge|timer|text) ([^ ]+)(?: {([^}]+)})?(?: (\S+))?(?: (.+))?`)

// FindMetricOrNil returns a metric in a store, or returns nil if not found.
func FindMetricOrNil(store *metrics.Store, name string) *metrics.Metric {
	store.RLock()
	defer store.RUnlock()
	for n, ml := range store.Metrics {
		if n == name {
			return ml[0]
		}
	}
	return nil
}

// ReadTestData loads a "golden" test data file, for a programfile, into the provided store.
func ReadTestData(file io.Reader, programfile string, store *metrics.Store) {
	prog := filepath.Base(programfile)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		glog.V(2).Infof("'%s'\n", scanner.Text())
		match := varRe.FindStringSubmatch(scanner.Text())
		glog.V(2).Infof("len match: %d\n", len(match))
		if len(match) == 0 {
			continue
		}
		keys := make([]string, 0)
		vals := make([]string, 0)
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
		var kind metrics.Kind
		switch match[1] {
		case "counter":
			kind = metrics.Counter
		case "gauge":
			kind = metrics.Gauge
		case "timer":
			kind = metrics.Timer
		case "text":
			kind = metrics.Text
		}
		glog.V(2).Infof("match[4]: %q", match[4])
		typ := datum.Int
		var (
			ival int64
			fval float64
			sval string
			err  error
		)
		if match[4] != "" {
			ival, err = strconv.ParseInt(match[4], 10, 64)
			if err != nil {
				fval, err = strconv.ParseFloat(match[4], 64)
				typ = datum.Float
				if err != nil || fval == 0.0 {
					sval = match[4]
					err = nil
					typ = datum.String
				}
			}
		}
		var timestamp time.Time
		glog.V(2).Infof("match 5: %q\n", match[5])
		if match[5] != "" {
			timestamp, _ = time.Parse(time.RFC3339, match[5])
		}
		glog.V(2).Infof("timestamp is %s which is %v in unix", timestamp.Format(time.RFC3339), timestamp.Unix())

		// Now we have enough information to get orcreate a metric.
		m := FindMetricOrNil(store, match[2])
		if m != nil {
			if m.Type != typ {
				glog.V(2).Infof("The type of the fetched metric is not %s: %s", typ, m)
				continue
			}
		} else {
			m = metrics.NewMetric(match[2], prog, kind, typ, keys...)
			if kind == metrics.Counter && len(keys) == 0 {
				d, err := m.GetDatum()
				if err != nil {
					glog.Fatal(err)
				}
				// Initialize to zero at the zero time.
				switch typ {
				case metrics.Int:
					datum.SetInt(d, 0, time.Unix(0, 0))
				case metrics.Float:
					datum.SetFloat(d, 0, time.Unix(0, 0))
				}
			}
			glog.V(2).Infof("making a new %v\n", m)
			if err := store.Add(m); err != nil {
				glog.Infof("Failed to add metric %v to store: %s", m, err)
			}
		}

		if match[4] != "" {
			d, err := m.GetDatum(vals...)
			if err != nil {
				glog.V(2).Infof("Failed to get datum: %s", err)
				continue
			}
			glog.V(2).Infof("got datum %v", d)

			switch typ {
			case metrics.Int:
				glog.V(2).Infof("setting %v with vals %v to %v at %v\n", d, vals, ival, timestamp)
				datum.SetInt(d, ival, timestamp)
			case metrics.Float:
				glog.V(2).Infof("setting %v with vals %v to %v at %v\n", d, vals, fval, timestamp)
				datum.SetFloat(d, fval, timestamp)
			case metrics.String:
				glog.V(2).Infof("setting %v with vals %v to %v at %v\n", d, vals, sval, timestamp)
				datum.SetString(d, sval, timestamp)
			}
		}
		glog.V(2).Infof("Metric is now %s", m)
	}
}
