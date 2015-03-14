# Introduction

A metric is a data type that describes a measurement.

It has a **name**, and a **value**, and a **time** that the measurement was taken.

It also has **units**, so that measurements can be compared and calculated with.

It has a **type**, so that tools can automatically perform some aggregation operations on collections of measurements.

Finally, it has some **tags**, so that additional information about the measurement can be added to assist queries later.

# Details

```
type Metric struct {
    name string
    value float64
    time time.Time
    typ metricType  // COUNTER, GAUGE
    unit string
}
```

## Typed Metrics

The type of a Metric can be:

  * a monotonically increasing counter, that allows the calculation of rates of change
  * a variable gauge, that records instantaneous values

Counters are very powerful as they are resistant to errors caused by sampling frequency.  Typically used to accumulate events, they can show changes in behaviour through the calculation of rates, and rates of rates.  They can be summed across a group and that sum also derived.  Counter resets can indicate crashes or restarts.

Gauges are less powerful as their ability to report is dependent on the sampling rate -- spikes in the timeseries can be missed.  They record queue lengths, resource usage and quota, and other sized measurements.

(N.B. Gauges can be simulated with two counters.)