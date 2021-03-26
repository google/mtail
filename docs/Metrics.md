# Metrics in `mtail`

## Introduction

A metric is a data type that describes a measurement.

It has a **name**, and a **value**, and a **time** that the measurement was taken.

It also has **units**, so that measurements can be compared and calculated with.

It has a **class**, so that tools can automatically perform some aggregation operations on collections of measurements.

It has a **type**, describing the sort of data it contains: floating point or integer values.

Finally, it has some **labels**, so that additional information about the measurement can be added to assist queries later.  Labels are key/value pairs, where the value may change for a specific measurement, but the keys remain constant across all measurements in a metric.

## Classes of Metrics

The class of a Metric can be:

  * a monotonically increasing counter, that allows the calculation of rates of change
  * a variable gauge, that records instantaneous values

Counters are very powerful as they are resistant to errors caused by sampling frequency.  Typically used to accumulate events, they can show changes in behaviour through the calculation of rates, and rates of rates.  They can be summed across a group and that sum also derived.  Counter resets can indicate crashes or restarts.

Gauges are less powerful as their ability to report is dependent on the sampling rate -- spikes in the timeseries can be missed.  They record queue lengths, resource usage and quota, and other sized measurements.

(N.B. Gauges can be simulated with two counters.)

## Types of data

`mtail` records either integer or floating point values as the value of a metric.  By default, all metrics are integer, unless the compiler can infer a floating point type.

Inference is done through the type checking pass of the compiler.  It uses knowledge of the expressions written in the program as well as heuristics on capturing groups in the regular expressions given.

For example, in the program:

```
counter a

/(\S+)/ {
  a = $1
}
```

the compiler will assume that `a` is of an integer type.  With more information about the matched text:

```
counter a

/(\d+\.\d+)/ {
  a = $1
}
```

the compiler can figure out that the capturing group reference `$1` contains digit and decimal point characters, and is likely then a floating point type.

## Labelling

Labels are added as dimensions on a metric:

```
counter a by x, y, z
```

creates a three dimensional metric called `a`, with each dimension key `x`, `y`, `z`.

Setting a measurement by label is done with an indexed expression:

```
  a[1, 2, 3]++
```

which has the effect of incrementing the metric a when x = 1, y = 2, and z = 3.

Dimensions, aka *labels* in the metric name, can be used to export rich data to
the metrics collector, for potential slicing and aggregation by each dimension.
