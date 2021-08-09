mtail - extract internal monitoring data from application logs for collection into a timeseries database
========================================================================================================

`mtail` is a tool for extracting metrics from application logs to be exported
into a timeseries database or timeseries calculator for alerting and
dashboarding.

It fills a monitoring niche by being the glue between applications that do not
export their own internal state (other than via logs) and existing monitoring
systems, such that system operators do not need to patch those applications to
instrument them or writing custom extraction code for every such application.

The extraction is controlled by [mtail programs](Programming-Guide.md)
which define patterns and actions:

    # simple line counter
    counter lines_total
    /$/ {
      lines_total++
    }

Metrics are exported for scraping by a collector as JSON or Prometheus format
over HTTP, or can be periodically sent to a collectd, StatsD, or Graphite
collector socket.

Read the [programming guide](Programming-Guide.md) if you want to learn how
to write mtail programs.

Ask general questions on the users mailing list: https://groups.google.com/g/mtail-users

## Table of Contents

* [Building `mtail`](Building.md)
* [Deploying `mtail`](Deploying.md)
  * [Interoperability](Interoperability.md)
  * [Troubleshooting](Troubleshooting.md)
* [Programming Guide](Programming-Guide.md)
  * [Language](Language.md)
  * [Metrics](Metrics.md)
  * [Tracking State](state.md)
  * [Testing](Testing.md)
* [Contributing to `mtail`](style.md)
  * [Debugging](debugging.md)
* [FAQ](faq.md)
