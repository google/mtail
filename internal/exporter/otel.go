// Copyright 2025 The mtail Authors.  All Rights Reserved
// This file is available under the Apache license.

package exporter

import (
	"context"
	"fmt"
	"maps"
	"math"
	"slices"
	"time"

	"github.com/jaqx0r/mtail/internal/metrics"
	"github.com/jaqx0r/mtail/internal/metrics/datum"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetricgrpc"
	"go.opentelemetry.io/otel/sdk/instrumentation"
	"go.opentelemetry.io/otel/sdk/metric"
	"go.opentelemetry.io/otel/sdk/metric/metricdata"
	"go.opentelemetry.io/otel/sdk/resource"
)

var processStartTime = time.Now()

// Produce implements the opentelemetry Producer.Produce method.
func (e *Exporter) Produce(context.Context) ([]metricdata.ScopeMetrics, error) {
	scopedOtelMetrics := make(map[string][]metricdata.Metrics)

	e.store.Range(func(m *metrics.Metric) error {
		m.RLock()
		defer m.RUnlock()

		newMetric := metricdata.Metrics{
			Name:        m.Name,
			Description: fmt.Sprintf("%s defined at %s", m.Name, m.Source),
		}
		switch m.Kind {
		case metrics.Counter:
			switch m.Type {
			case metrics.Int:
				newMetric.Data = otelIntCounter(m)
			default:
				return nil
			}
		case metrics.Gauge:
			switch m.Type {
			case metrics.Int:
				newMetric.Data = otelIntGauge(m)
			case metrics.Float:
				newMetric.Data = otelFloatGauge(m)
			default:
				return nil
			}
		case metrics.Timer:
			switch m.Type {
			case metrics.Float:
				newMetric.Data = otelFloatGauge(m)
			default:
				return nil
			}
		case metrics.Histogram:
			switch m.Type {
			case metrics.Buckets:
				newMetric.Data = otelHisto(m)
			default:
				return nil
			}
		default:
			return nil
		}
		scopedOtelMetrics[m.Program] = append(scopedOtelMetrics[m.Program], newMetric)
		return nil
	})

	if len(scopedOtelMetrics) == 0 {
		return nil, nil
	}

	otelMetrics := make([]metricdata.ScopeMetrics, 0, len(scopedOtelMetrics))
	for _, prog := range slices.Sorted(maps.Keys(scopedOtelMetrics)) {
		otelMetrics = append(otelMetrics, metricdata.ScopeMetrics{Scope: instrumentation.Scope{Name: prog},
			Metrics: scopedOtelMetrics[prog],
		})
	}
	return otelMetrics, nil
}

func otelIntCounter(m *metrics.Metric) metricdata.Sum[int64] {
	counter := metricdata.Sum[int64]{
		DataPoints:  make([]metricdata.DataPoint[int64], 0, len(m.LabelValues)),
		Temporality: metricdata.CumulativeTemporality,
		IsMonotonic: true,
	}
	lsc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lsc)
	for ls := range lsc {
		dp := metricdata.DataPoint[int64]{
			Attributes: otelLabels(ls.Labels),
			StartTime:  processStartTime,
			Time:       ls.Datum.TimeUTC(),
			Value:      datum.GetInt(ls.Datum),
		}
		counter.DataPoints = append(counter.DataPoints, dp)
	}
	return counter
}

func otelIntGauge(m *metrics.Metric) metricdata.Gauge[int64] {
	gauge := metricdata.Gauge[int64]{
		DataPoints: make([]metricdata.DataPoint[int64], 0, len(m.LabelValues)),
	}
	lsc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lsc)
	for ls := range lsc {
		dp := metricdata.DataPoint[int64]{
			Attributes: otelLabels(ls.Labels),
			StartTime:  processStartTime,
			Time:       ls.Datum.TimeUTC(),
			Value:      datum.GetInt(ls.Datum),
		}
		gauge.DataPoints = append(gauge.DataPoints, dp)
	}
	return gauge
}

func otelFloatGauge(m *metrics.Metric) metricdata.Gauge[float64] {
	gauge := metricdata.Gauge[float64]{
		DataPoints: make([]metricdata.DataPoint[float64], 0, len(m.LabelValues)),
	}
	lsc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lsc)
	for ls := range lsc {
		dp := metricdata.DataPoint[float64]{
			Attributes: otelLabels(ls.Labels),
			StartTime:  processStartTime,
			Time:       ls.Datum.TimeUTC(),
			Value:      datum.GetFloat(ls.Datum),
		}
		gauge.DataPoints = append(gauge.DataPoints, dp)
	}
	return gauge
}

func otelHisto(m *metrics.Metric) metricdata.Histogram[float64] {
	histo := metricdata.Histogram[float64]{
		DataPoints:  make([]metricdata.HistogramDataPoint[float64], 0, len(m.LabelValues)),
		Temporality: metricdata.CumulativeTemporality,
	}
	lsc := make(chan *metrics.LabelSet)
	go m.EmitLabelSets(lsc)
	for ls := range lsc {
		bounds, counts := otelConvertBuckets(datum.GetBuckets(ls.Datum))
		dp := metricdata.HistogramDataPoint[float64]{
			Attributes:   otelLabels(ls.Labels),
			StartTime:    processStartTime,
			Time:         ls.Datum.TimeUTC(),
			Count:        datum.GetBucketsCount(ls.Datum),
			Sum:          datum.GetBucketsSum(ls.Datum),
			Bounds:       bounds,
			BucketCounts: counts,
		}
		histo.DataPoints = append(histo.DataPoints, dp)
	}
	return histo
}

func otelConvertBuckets(d *datum.Buckets) (bounds []float64, counts []uint64) {
	if len(d.Buckets) == 0 {
		// Should never happen?
		return nil, nil
	}
	// The last bucket may be the +Inf bucket, which is implied in OTel, but explicit in mtail.
	if math.IsInf(d.Buckets[len(d.Buckets)-1].Range.Max, +1) {
		bounds = make([]float64, len(d.Buckets)-1)
	} else {
		bounds = make([]float64, len(d.Buckets))
	}
	counts = make([]uint64, len(d.Buckets))
	for i, bucket := range d.Buckets {
		if bound := bucket.Range.Max; !math.IsInf(bound, +1) {
			bounds[i] = bound
		}
		counts[i] = bucket.Count
	}
	return
}

func otelLabels(labels map[string]string) attribute.Set {
	l := len(labels)
	kvs := make([]attribute.KeyValue, l)
	i := 0
	for k, v := range labels {
		kvs[i] = attribute.String(k, v)
		i++
	}
	return attribute.NewSet(kvs...)
}

func (e *Exporter) InitOtel(ctx context.Context) error {
	res, err := resource.New(ctx,
		resource.WithHost(),
		resource.WithAttributes(
			attribute.String("service.name", "mtail"),
			attribute.String("service.version", e.version),
		))
	if err != nil {
		return err
	}
	otlpexp, err := otlpmetricgrpc.New(ctx, otlpmetricgrpc.WithInsecure(), otlpmetricgrpc.WithTimeout(*writeDeadline))
	if err != nil {
		return err
	}
	reader := metric.NewPeriodicReader(otlpexp, metric.WithInterval(e.pushInterval), metric.WithProducer(e))
	meterProvider := metric.NewMeterProvider(metric.WithReader(reader), metric.WithResource(res))
	otel.SetMeterProvider(meterProvider)

	e.wg.Add(1)
	// Shut down the otel meter provider at exit
	go func() {
		defer e.wg.Done()
		<-e.ctx.Done()
		meterProvider.Shutdown(ctx)
	}()

	return nil
}
