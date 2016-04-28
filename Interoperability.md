# Introduction

mtail is only part of a monitoring ecosystem -- it fills the gap between applications that export no metrics of their own in a [common protocol](Metrics) and the timeseries database.

# Details

mtail actively exports (i.e. pushes) to the following timeseries databases:

  * [collectd](http://collectd.org/)
  * [graphite](http://graphite.wikidot.com/start)
  * [statsd](https://github.com/etsy/statsd)
  
mtail also is a passive exporter (i.e. pull, or scrape based) by:

  * [Prometheus](http://prometheus.io)
  * Google's Borgmon
  * JSON over HTTP custom scripts.


# Logs Analysis

While mtail does a form of logs analysis, it does _not_ do any copying,
indexing, or searching of log files for data mining applications.  It is only
intended for real- or near-time monitoring data for the purposes of performance
measurement and alerting.

Instead, see logs ingestion and analysis systems like

  * [Logstash](https://www.elastic.co/products/logstash)
  * [Graylog](https://www.graylog.org/)
  
if that is what you need.
