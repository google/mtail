# Introduction

mtail is only part of a monitoring ecosystem -- it fills the gap between applications that export no metrics of their own in a [common protocol](Metrics) and the timeseries database.

# Details

mtail exports to the following timeseries databases:

  * [collectd](http://collectd.org/)
  * [graphite](http://graphite.wikidot.com/start)
  * [statsd](https://github.com/etsy/statsd)

Some other databases, which would require glue code:
  * [Saturnalia](https://bitbucket.org/0x0000/saturnalia/wiki/Home)
  * [openTSDB](http://opentsdb.net/)


# Logs Analysis

While mtail does a form of logs analysis, it does _not_ do any copying, indexing, or searching of log files for data mining applications.  It is only intended for real- or near-time monitoring data.
