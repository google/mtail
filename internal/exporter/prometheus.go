// Copyright 2015 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"expvar"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/metrics"
	"github.com/google/mtail/internal/metrics/datum"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/common/expfmt"
)

var metricExportTotal = expvar.NewInt("metric_export_total")

func noHyphens(s string) string {
	return strings.ReplaceAll(s, "-", "_")
}

// Describe implements the prometheus.Collector interface.
func (e *Exporter) Describe(c chan<- *prometheus.Desc) {
	prometheus.DescribeByCollect(e, c)
}

// Collect implements the prometheus.Collector interface.
func (e *Exporter) Collect(c chan<- prometheus.Metric) {
	lastMetric := ""
	lastSource := ""

	/* #nosec G104 always retursn nil */
	e.store.Range(func(m *metrics.Metric) error {
		m.RLock()
		// We don't have a way of converting text metrics to prometheus format.
		if m.Kind == metrics.Text {
			m.RUnlock()
			return nil
		}
		metricExportTotal.Add(1)
		glog.Infof("PYW source= %s, name= %s, kind= %s, type= %s", m.Source, m.Name, m.Kind, m.Type)

		lsc := make(chan *metrics.LabelSet)
		go m.EmitLabelSets(lsc)
		counter := 0
		isGaugeArray := false
		var gvec *prometheus.GaugeVec	// optional for collection performance ?
		for ls := range lsc {
			if lastMetric != m.Name {
				glog.V(2).Infof("setting source to %s", m.Source)
				lastSource = m.Source
				lastMetric = m.Name
			}
			var keys []string
			var vals []string
			for k, v := range ls.Labels {
				if k == "audit_count"  { // ignore this label
					isGaugeArray = true
				} else {
					keys = append(keys, k)
					vals = append(vals, v)
				}
			}
			if !e.omitProgLabel && !isGaugeArray {
				keys = append(keys, "prog")
				vals = append(vals, m.Program)
				// add my label
//				keys = append(keys, "owner")
//				vals = append(vals, "phil")
			}
			glog.Info("PYW gvec= ", gvec, ", isGaugeArray= ", isGaugeArray, ", counter=", counter, ", kind=", m.Kind)
			/* create and register a gauge vector
			if isGaugeArray && gvec == nil {
				gvec = prometheus.NewGaugeVec(
					prometheus.GaugeOpts{
//						Namespace: "splunk",
//						Subsystem: "search",
						Name:      "splunkd_mtail_audit_log_search_runtime",
						Help:      "defined at audit.mtail:23:11-58",
					},
//					prometheus.NewDesc(noHyphens(m.Name),
//						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					keys)
				prometheus.MustRegister(gvec)
			}
			*/
			var pM prometheus.Metric
			var err error
			if m.Kind == metrics.Histogram {
				pM, err = prometheus.NewConstHistogram(
					prometheus.NewDesc(noHyphens(m.Name),
						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					datum.GetBucketsCount(ls.Datum),
					datum.GetBucketsSum(ls.Datum),
					datum.GetBucketsCumByMax(ls.Datum),
					vals...)
			/* generate a new metric from the gauge vector
			} else if m.Kind == metrics.Gauge {
				if isGaugeArray {
//					pM, err = gvec.WithLabelValues(vals).Set(promValueForDatum(ls.Datum))
					pG := gvec.WithLabelValues("123")
//					pG := gvec.With(ls.Labels)
					pG.Set(promValueForDatum(ls.Datum))
					pM = pG
				} else {
					pM, err = prometheus.NewConstMetric(
					prometheus.NewDesc(noHyphens(m.Name),
						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					promTypeForKind(m.Kind),
					promValueForDatum(ls.Datum),
					vals...)
				}
			*/
			} else {
				pM, err = prometheus.NewConstMetric(
					prometheus.NewDesc(noHyphens(m.Name),
						fmt.Sprintf("defined at %s", lastSource), keys, nil),
					promTypeForKind(m.Kind),
					promValueForDatum(ls.Datum),
					vals...)
				/* reset the gauge to -1, optional
//				if isDatumFloat(ls.Datum) { 
				if isGaugeArray { 
					datum.SetFloat(ls.Datum, -1.0, ls.Datum.TimeUTC())
				}
				*/
			}
			if err != nil {
				glog.Warning(err)
				return nil
			}
			// By default no timestamp is emitted to Prometheus. Setting a
			// timestamp is not recommended. It can lead to unexpected results
			// if the timestamp is not updated or moved fowarded enough to avoid
			// triggering Promtheus staleness handling.
			// Read more in docs/faq.md
			if e.emitTimestamp || isGaugeArray {
				// use a new timestamp to emit the guage array, one by one
				duration := time.Second * time.Duration(counter)
				timeUTC := ls.Datum.TimeUTC().Add(duration)
				c <- prometheus.NewMetricWithTimestamp(timeUTC, pM)
			} else {
				c <- pM
			}
			counter++
		}
		glog.Info("PYW chan= ", len(c), ", isGaugeArray= ", isGaugeArray, ", counter=", counter)
		// clean up the metric store
		if isGaugeArray {
//			m.RemoveOldestDatum()	// oldest
			m.ClearGaugeDataLocked()
		}
		m.RUnlock()
		return nil
	})
}

// Write is used to write Prometheus metrics to an io.Writer.
func (e *Exporter) Write(w io.Writer) error {
	reg := prometheus.NewRegistry()
	err := reg.Register(e)
	if err != nil {
		return err
	}
	mfs, err := reg.Gather()
	if err != nil {
		return err
	}
	enc := expfmt.NewEncoder(w, expfmt.FmtText)
	for _, mf := range mfs {
		err := enc.Encode(mf)
		if err != nil {
			return err
		}
	}
	return nil
}

func promTypeForKind(k metrics.Kind) prometheus.ValueType {
	switch k {
	case metrics.Counter:
		return prometheus.CounterValue
	case metrics.Gauge:
		return prometheus.GaugeValue
	case metrics.Timer:
		return prometheus.GaugeValue
	}
	return prometheus.UntypedValue
}

func promValueForDatum(d datum.Datum) float64 {
	switch n := d.(type) {
	case *datum.Int:
		return float64(n.Get())
	case *datum.Float:
		return n.Get()
	}
	return 0.
}

func isDatumFloat(d datum.Datum) bool {
	switch n := d.(type) {
	case *datum.Float:
		_ = n
		return true
	}
	return false
}

