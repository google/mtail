package golden

import (
	"os"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/google/mtail/internal/testutil"
)

var expectedMetrics = map[string][]*metrics.Metric{
	"bytes_total": {
		&metrics.Metric{
			Name:    "bytes_total",
			Program: "reader_test",
			Kind:    metrics.Counter,
			Keys:    []string{"operation"},
			LabelValues: []*metrics.LabelValue{
				{
					Labels: []string{"sent"},
					Value:  datum.MakeInt(62793673, time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC))},
				{
					Labels: []string{"received"},
					Value:  datum.MakeInt(975017, time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC))}}},
	},
	"connections_total": {
		&metrics.Metric{
			Name:    "connections_total",
			Program: "reader_test",
			Kind:    metrics.Counter,
			Keys:    []string{},
			LabelValues: []*metrics.LabelValue{
				{
					Value: datum.MakeInt(52, time.Date(2011, 2, 22, 21, 54, 13, 0, time.UTC))}}},
	},
	"connection-time_total": {
		&metrics.Metric{
			Name:    "connection-time_total",
			Program: "reader_test",
			Kind:    metrics.Counter,
			Keys:    []string{},
			LabelValues: []*metrics.LabelValue{
				{
					Value: datum.MakeInt(1181011, time.Date(2011, 2, 23, 5, 54, 10, 0, time.UTC))}}},
	},
	"transfers_total": {
		&metrics.Metric{
			Name:    "transfers_total",
			Program: "reader_test",
			Kind:    metrics.Counter,
			Keys:    []string{"operation", "module"},
			LabelValues: []*metrics.LabelValue{
				{
					Labels: []string{"send", "module"},
					Value:  datum.MakeInt(2, time.Date(2011, 2, 23, 5, 50, 32, 0, time.UTC))},
				{
					Labels: []string{"send", "repo"},
					Value:  datum.MakeInt(25, time.Date(2011, 2, 23, 5, 51, 14, 0, time.UTC))}}},
	},
	"foo": {
		&metrics.Metric{
			Name:        "foo",
			Program:     "reader_test",
			Kind:        metrics.Gauge,
			Keys:        []string{"label"},
			LabelValues: []*metrics.LabelValue{},
		},
	},
	"bar": {
		&metrics.Metric{
			Name:    "bar",
			Program: "reader_test",
			Kind:    metrics.Counter,
			Keys:    []string{},
			LabelValues: []*metrics.LabelValue{
				{
					Value: datum.MakeInt(0, time.Unix(0, 0)),
				},
			},
		},
	},
	"floaty": {
		&metrics.Metric{
			Name:    "floaty",
			Program: "reader_test",
			Kind:    metrics.Gauge,
			Type:    metrics.Float,
			Keys:    []string{},
			LabelValues: []*metrics.LabelValue{
				{
					Labels: []string{},
					Value:  datum.MakeFloat(37.1, time.Date(2017, 6, 15, 18, 9, 37, 0, time.UTC)),
				},
			},
		},
	},
	"stringy": {
		&metrics.Metric{
			Name:    "stringy",
			Program: "reader_test",
			Kind:    metrics.Text,
			Type:    metrics.String,
			Keys:    []string{},
			LabelValues: []*metrics.LabelValue{
				{
					Labels: []string{},
					Value:  datum.MakeString("hi", time.Date(2018, 6, 16, 18, 04, 0, 0, time.UTC)),
				},
			},
		},
	},
}

func TestReadTestData(t *testing.T) {
	f, err := os.Open("reader_test.golden")
	testutil.FatalIfErr(t, err)
	defer f.Close()
	store := metrics.NewStore()
	ReadTestData(f, "reader_test", store)
	testutil.ExpectNoDiff(t, expectedMetrics, store.Metrics, testutil.IgnoreUnexported(sync.RWMutex{}, datum.String{}))
}
