# Interoperability of `mtail` with other monitoring tools

## Introduction

`mtail` is only part of a monitoring ecosystem -- it fills the gap between applications that export no metrics of their own in a [common protocol](Metrics.md) and the timeseries database.

`mtail` is intended to complement other tools to build a complete system, and usually does not try to add functionality better provided by systems specifically designed for that function.

# Metric export and collection

mtail actively exports (i.e. pushes) to the following timeseries databases:

  * [collectd](http://collectd.org/)
  * [graphite](http://graphite.wikidot.com/start)
  * [statsd](https://github.com/etsy/statsd)

mtail also is a passive exporter (i.e. pull, or scrape based) by:

  * [Prometheus](http://prometheus.io)
  * Google's Borgmon
  
*Recommendation*

Of the above, `mtail` recommends using Prometheus to extract the metrics from mtail as it is a rich monitoring tool and has a lot of interoperability itself.  The `collectd`, `graphite`, and `statsd` options are less battle-tested and originate from an earlier time when the industry had not yet crystallised around a metric protocol.

No configuration is required to enable Prometheus export from `mtail`.

## Prometheus Exporter Metrics

Prometheus' [writing exporters documentation](https://prometheus.io/docs/instrumenting/writing_exporters/) describes useful metrics for a Prometheus exporter to export. `mtail` does not follow that guide, for these reasons.

The exporter model described in that document is for active proxies between an application and Prometheus.  The expectation is that when Prometheus scrapes the proxy (the exporter) that it then performs its own scrape of the target application, and translates the results back into the Prometheus exposition format.  The time taken to query the target application is what is exported as `X_scrape_duration_seconds` and its availability as `X_up`.

`mtail` doesn't work like that.  It is reacting to the input log events, not scrapes, and so there is no concept of how long it takes to query the application or if it is available.  There are things that, if you squint, look like applications in `mtail`, the virtual machine programs.  They could be exporting their time to process a single line, and are `up` as long as they are not crashing on input.  This doesn't translate well into the exporter metrics meanings though.

TODO(jaq): Instead, mtail will export a histogram of the runtime per line of each VM program.

`mtail` doesn't export `mtail_up` or `mtail scrape_duration_seconds` because they are exactly equivalent* to the [synthetic metrics](https://prometheus.io/docs/concepts/jobs_instances/) that Prometheus creates automatically.

\* The difference between a scrape duration measured in mtail versus Prometheus would differ in the network round trip time, TCP setup time, and send/receive queue time.  For practical purposes you can ignore them as the usefulness of a scrape duration metric is not in its absolute value, but how it changes over time.


# Log Collection, Distribution, and Filtering {: #syslog}

`mtail` is not intended to be used as a replacement for `syslogd`.  `mtail` can read from named pipes and unix domain sockets on systems that support them, but the intent is that a proper `syslogd` can manage the collection of those logs, filter out interesting ones if necessary, and forward them to `mtail` via a named pipe.

Both `rsyslogd` and `syslog-ng` are possible choices here.

It's probably not a good idea to have `mtail` listen directly to `/dev/log` or read from `/run/systemd/journal/syslog` unless you know what you're doing.  `mtail` does not want to be in the business of API specialisation, but `syslog-ng` has done so with its [`system()` family of collector configuration options](https://www.syslog-ng.com/technical-documents/doc/syslog-ng-open-source-edition/3.22/administration-guide/26#TOPIC-1209162).

* rsyslog supports forwarding to a [named pipe](https://www.rsyslog.com/doc/master/configuration/modules/ompipe.html) and to a [unix domain socket](https://www.rsyslog.com/doc/master/configuration/modules/omuxsock.html)

* syslog-ng supports output to [named pipe](https://www.syslog-ng.com/technical-documents/doc/syslog-ng-open-source-edition/3.30/administration-guide/44#TOPIC-1595018) and [unix domain socket](https://www.syslog-ng.com/technical-documents/doc/syslog-ng-open-source-edition/3.30/administration-guide/54#TOPIC-1595060)

Additionally, use a proper syslog to transmit and receive logs over the network.  `mtail` does not provide any transport security, nor does TCP itself guarantee that no loss of data will occur: the [RELP spec](https://www.rsyslog.com/doc/v8-stable/tutorials/reliable_forwarding.html) exists for the latter.

*Recommendation*

Run `mtail` with a `--logs unix:///run/mtail.sock` flag to specify a single unix domain socket, or `mkfifo /run/mtail.pipe` to create a named pipe and `--logs /run/mtail.pipe` to share between `mtail` and the syslog daemon.  Instruct the syslog daemon to forward syslog to the socket or pipe so named with one of the options described above (or as documented by your syslog daemon manual.)

# Logs Analysis

While `mtail` does a form of logs analysis, it does _not_ do any copying,
indexing, or searching of log files for data mining applications.  It is only
intended for real- or near-time monitoring data for the purposes of performance
measurement and alerting.

Instead, see logs ingestion and analysis systems like

  * [Logstash](https://www.elastic.co/products/logstash)
  * [Graylog](https://www.graylog.org/)

if that is what you need.

*Recommendation*

`mtail` provides no recommendations here as there is no direct interoperation between `mtail` and logs analysis.  The interface to logs analysis will be from the syslog daemon or application logger directly.  If a logs analysis collector is receiving application logs, then `mtail` is either running concurrently reading those application logs as well, or the logs analysis collector is teeing to `mtail` in a manner similar to syslog daemons above.


# TLS/SSL {: #tls-ssl}

Sometimes one may wish to expose `mtail` directly to the internet, but would like to protect it from unauthorized access. `mtail` doesn't support SSL or HTTP authentication, and should be used with a VPN tunnel or reverse proxy instead.

Assuming a VPN tunnel is out of the question, then termination of SSL connections is possible with tools like [`nginx`]() and [`varnish`]().

`mtail` can listen on either a TCP socket or a unix domain socket for HTTP requests; the latter is done with `--unix_socket` instead of the `--address` and `--port` flags.

Forwarding to a unix domain socket instead of TCP is possible with both [`nginx`](http://nginx.org/en/docs/http/ngx_http_upstream_module.html) and [`varnish`](https://varnish-cache.org/docs/trunk/whats-new/upgrading-6.0.html#upd-6-0-uds-backend).

*Recommendation*

If no VPN tunnel is possible, then use a reverse proxy to terminate HTTPS and then forward to `mtail` over a unix domain socket, by setting the `--unix_socket /run/mtail.http.sock` and then configuring the reverse proxy to use the unix socket as a backend.
