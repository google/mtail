package testdata

import (
	"bufio"
	"fmt"
	"io"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/metrics"
)

var var_re = regexp.MustCompile(`^(counter|gauge|timer) ([^ ]+)( {([^}]+)})? (\d+)( (.+))?`)

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
		fmt.Printf("'%s'\n", scanner.Text())
		match := var_re.FindStringSubmatch(scanner.Text())
		fmt.Printf("len match: %d\n", len(match))
		if len(match) == 0 {
			continue
		}
		var keys, vals []string
		if match[3] != "" {
			for _, pair := range strings.Split(match[4], ",") {
				fmt.Printf("pair: %s\n", pair)
				kv := strings.Split(pair, "=")
				keys = append(keys, kv[0])
				vals = append(vals, kv[1])
			}
		}
		var timestamp time.Time
		fmt.Printf("match 7: %q\n", match[7])
		if match[7] != "" {
			timestamp, _ = time.Parse(time.RFC3339, match[7])
		}
		fmt.Printf("timestamp is %s which is %v in unix\n", timestamp.Format(time.RFC3339), timestamp.Unix())

		val, err := strconv.ParseInt(match[5], 10, 64)
		if err != nil {
			glog.Fatalf("parse failed for '%s': %s", match[5], err)
		}
		m := FindMetricOrNil(store, match[2])
		if m == nil {
			var kind metrics.MetricType
			switch match[1] {
			case "counter":
				kind = metrics.Counter
			case "gauge":
				kind = metrics.Gauge
			case "timer":
				kind = metrics.Timer
			}
			m = metrics.NewMetric(match[2], prog, kind, keys...)
			fmt.Printf("making a new %v\n", m)
			store.Add(m)
		} else {
			fmt.Printf("found %v\n", m)
		}
		d, err := m.GetDatum(vals...)
		if err != nil {
			fmt.Printf("Failed to get datum: %s\n", err)
			continue
		}
		fmt.Printf("setting %v with vals %v to %v\n", d, vals, val)
		d.Set(val, timestamp)
	}
}
