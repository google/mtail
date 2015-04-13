mtail - extract whitebox monitoring data from application logs for collection into a timeseries database
========================================================================================================

[![GoDoc](https://godoc.org/github.com/google/mtail?status.png)](http://godoc.org/github.com/google/mtail)
[![Travis-CI](https://travis-ci.org/google/mtail.svg)]

mtail is a tool for extracting metrics from application logs to be exported into a timeseries database or timeseries calculator for alerting and dashboarding.

It aims to fill a niche between applications that do not export their own internal state, and existing monitoring systems, without patching those applications or rewriting the same framework for custom extraction glue code.

The extraction is controlled by mtail programs which define patterns and actions:

    # simple line counter
    counter line_count
    /$/ {
      line_count++
    }

Metrics are exported for scraping by a collector as JSON or Prometheus format over HTTP, or can be periodically sent to a collectd, statsd, or Graphite collector socket.

See [the Wiki](https://github.com/google/mtail/wiki/Home) for more details.

Mailing list: https://groups.google.com/forum/#!forum/mtail-users
