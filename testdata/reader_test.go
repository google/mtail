package testdata

import (
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/google/mtail/metrics"
	"github.com/kylelemons/godebug/pretty"
)

var expectedMetrics = []metrics.Metric{
	metrics.Metric{
		Name:    "bytes_total",
		Program: "reader_test",
		Kind:    metrics.Counter,
		Keys:    []string{"operation"},
		LabelValues: []*metrics.LabelValue{
			&metrics.LabelValue{
				Labels: []string{"sent"},
				Value:  &metrics.Datum{Value: 62793673, Time: time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC)}},
			&metrics.LabelValue{
				Labels: []string{"received"},
				Value:  &metrics.Datum{Value: 975017, Time: time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC)}}}},
	metrics.Metric{
		Name:    "connections_total",
		Program: "reader_test",
		Kind:    metrics.Counter,
		Keys:    []string{},
		LabelValues: []*metrics.LabelValue{
			&metrics.LabelValue{
				Value: &metrics.Datum{Value: 52, Time: time.Date(2011, 2, 22, 21, 54, 13, 0, time.UTC)}}}},
	metrics.Metric{
		Name:    "connection-time_total",
		Program: "reader_test",
		Kind:    metrics.Counter,
		LabelValues: []*metrics.LabelValue{
			&metrics.LabelValue{
				Value: &metrics.Datum{Value: 1181011, Time: time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC)}}}},
	metrics.Metric{
		Name:    "transfers_total",
		Program: "reader_test",
		Kind:    metrics.Counter,
		Keys:    []string{"operation", "module"},
		LabelValues: []*metrics.LabelValue{
			&metrics.LabelValue{
				Labels: []string{"send", "module"},
				Value:  &metrics.Datum{Value: 2, Time: time.Date(2011, 2, 23, 5, 50, 32, 0, time.UTC)}},
			&metrics.LabelValue{
				Labels: []string{"send", "repo"},
				Value:  &metrics.Datum{Value: 25, Time: time.Date(2011, 2, 23, 5, 51, 14, 0, time.UTC)}}}},
	metrics.Metric{
		Name:    "foo",
		Program: "reader_test",
		Kind:    metrics.Gauge,
		Keys:    []string{"label"},
	},
}

func TestReadTestData(t *testing.T) {
	f, err := os.Open("reader_test.golden")
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	store := &metrics.Store{}
	ReadTestData(f, "reader_test", store)
	fmt.Printf("%v\n", store.Metrics)
	diff := pretty.Compare(expectedMetrics, store.Metrics)
	if len(diff) > 0 {
		t.Errorf("metrics don't match: %s\n", diff)
	}
}
