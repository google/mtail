# Interoperability of `mtail` with other monitoring tools

## Introduction

mtail is only part of a monitoring ecosystem -- it fills the gap between applications that export no metrics of their own in a [common protocol](Metrics.md) and the timeseries database.

# Details

mtail actively exports (i.e. pushes) to the following timeseries databases:

  * [collectd](http://collectd.org/)
  * [graphite](http://graphite.wikidot.com/start)
  * [statsd](https://github.com/etsy/statsd)

mtail also is a passive exporter (i.e. pull, or scrape based) by:

  * [Prometheus](http://prometheus.io)
  * Google's Borgmon


# Logs Analysis

While `mtail` does a form of logs analysis, it does _not_ do any copying,
indexing, or searching of log files for data mining applications.  It is only
intended for real- or near-time monitoring data for the purposes of performance
measurement and alerting.

Instead, see logs ingestion and analysis systems like

  * [Logstash](https://www.elastic.co/products/logstash)
  * [Graylog](https://www.graylog.org/)

if that is what you need.

# Prometheus Exporter Metrics

https://prometheus.io/docs/instrumenting/writing_exporters/ describes useful metrics for a Prometheus exporter to export. `mtail` does not follow that guide, for these reasons.

The exporter model described in that document is for active proxies between an application and Prometheus.  The expectation is that when Prometheus scrapes the proxy (the exporter) that it then performs its own scrape of the target application, and translates the results back into the Prometheus exposition format.  The time taken to query the target application is what is exported as `X_scrape_duration_seconds` and its availability as `X_up`.

`mtail` doesn't work like that.  It is reacting to the input log events, not scrapes, and so there is no concept of how long it takes to query the application or if it is available.  There are things that, if you squint, look like applications in `mtail`, the virtual machine programs.  They could be exporting their time to process a single line, and are `up` as long as they are not crashing on input.  This doesn't translate well into the exporter metrics meanings though.

TODO(jaq): Instead, mtail will export a histogram of the runtime per line of each VM program.

`mtail` doesn't export `mtail_up` or `mtail_scrape_duration_seconds` because they are exactly equivalent* the synthetic metrics that Prometheus creates automatically: https://prometheus.io/docs/concepts/jobs_instances/

\* The difference between a scrape duration measured in mtail versus Prometheus would differ in the network round trip time, TCP setup time, and send/receive queue time.  For practical purposes you can ignore them as the usefulness of a scrape duration metric is not in its absolute value, but how it changes over time.
