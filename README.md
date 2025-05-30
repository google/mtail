<img src="https://raw.githubusercontent.com/jaqx0r/mtail/main/logo.png" alt="mtail" title="mtail" align="right" width="140">

# mtail - extract internal monitoring data from application logs for collection into a timeseries database

[![ci](https://github.com/jaqx0r/mtail/workflows/CI/badge.svg)](https://github.com/jaqx0r/mtail/actions?query=workflow%3ACI+branch%3main)
[![GoDoc](https://godoc.org/github.com/jaqx0r/mtail?status.png)](http://godoc.org/github.com/jaqx0r/mtail)
[![Go Report Card](https://goreportcard.com/badge/github.com/jaqx0r/mtail)](https://goreportcard.com/report/github.com/jaqx0r/mtail)
[![OSS-Fuzz](https://oss-fuzz-build-logs.storage.googleapis.com/badges/mtail.svg)](https://bugs.chromium.org/p/oss-fuzz/issues/list?sort=-opened&can=1&q=proj:mtail)
[![codecov](https://codecov.io/gh/jaqx0r/mtail/branch/main/graph/badge.svg)](https://codecov.io/gh/jaqx0r/mtail)

`mtail` is a tool for extracting metrics from application logs to be exported
into a timeseries database or timeseries calculator for alerting and
dashboarding.

It fills a monitoring niche by being the glue between applications that do not
export their own internal state (other than via logs) and existing monitoring
systems, such that system operators do not need to patch those applications to
instrument them or writing custom extraction code for every such application.

The extraction is controlled by [mtail programs](https://jaqx0r.github.io/mtail/Programming-Guide)
which define patterns and actions:

    # simple line counter
    counter lines_total
    /$/ {
      lines_total++
    }

Metrics are exported for scraping by a collector as JSON or Prometheus format
over HTTP, or can be periodically sent to a collectd, StatsD, or Graphite
collector socket.

Read the [programming guide](https://jaqx0r.github.io/mtail/Programming-Guide) if you want to learn how
to write mtail programs.

Ask general questions on the users mailing list: https://groups.google.com/g/mtail-users

## Installation

There are various ways of installing **mtail**.

### Precompiled binaries

Precompiled binaries for released versions are available in the
[Releases page](https://github.com/jaqx0r/mtail/releases) on Github. Using the
latest production release binary is the recommended way of installing **mtail**.

Windows, OSX and Linux binaries are available.

### Building from source

`mtail` uses [`bazel`](https://bazel.build) for its speed, hermeticity, and support for cross-language compilation.  Install [`bazelisk`](https://bazel.build/install/bazelisk) to manage Bazel for you.

Then:

```
bazel build //cmd/mtail
```

See the [Build instructions](https://jaqx0r.github.io/mtail/Building) for more details.

The build system can also emit an OCI container image for you.

```
bazel build //:oci_image
```

will create it, but to run it the best option is to load it into your local container runtime

```
bazel run //:load_image
```

## Deployment

`mtail` works best when paired with a timeseries-based calculator and
alerting tool, like [Prometheus](http://prometheus.io).

### Source-to-Program Mapping

By default, mtail processes every log line with every loaded program. For large installations with many logs and programs, this can be inefficient. You can optimize performance by mapping specific log sources to specific programs via the `--source_mapping_file` option:

```
mtail --progs=/etc/mtail/progs --logs=/var/log/syslog,/var/log/apache2/*.log --source_mapping_file=/etc/mtail/source_mapping.yaml
```

This file can be in YAML or JSON format and allows you to specify which programs should process which log sources. See the `examples/source_mapping.yaml` and `examples/source_mapping.json` files for examples.

You can also control how to handle unmapped sources with the `--unmapped_behavior` flag. Valid values are "all" (process with all programs, the default) or "none" (don't process unmapped sources with any program).

> So what you do is you take the metrics from the log files and
> you bring them down to the monitoring system?

[It deals with the instrumentation so the engineers don't have
to!](http://www.imdb.com/title/tt0151804/quotes/?item=qt0386890)  It has the
extraction skills!  It is good at dealing with log files!!

Learn more about [interoperability with other tools](https://jaqx0r.github.io/mtail/Interoperability)

## Read More

Full documentation at https://jaqx0r.github.io/mtail/

Read more about writing `mtail` programs:

* [Programming Guide](https://jaqx0r.github.io/mtail/Programming-Guide)
* [Language Reference](https://jaqx0r.github.io/mtail/Language)
* [Metrics](https://jaqx0r.github.io/mtail/Metrics)
* [Managing internal state](https://jaqx0r.github.io/mtail/state)
* [Testing your programs](https://jaqx0r.github.io/mtail/Testing)

Read more about hacking on `mtail`

* [Building from source](https://jaqx0r.github.io/mtail/Building)
* [Contributing](CONTRIBUTING.md)
* [Style](https://jaqx0r.github.io/mtail/style)

Read more about deploying `mtail` and your programs in a monitoring environment

* [Deploying](https://jaqx0r.github.io/mtail/Deploying)
* [Interoperability](https://jaqx0r.github.io/mtail/Interoperability) with other systems
* [Troubleshooting](https://jaqx0r.github.io/mtail/Troubleshooting)
* [FAQ](https://jaqx0r.github.io/mtail/faq)


## Getting more help and reporting defects

If you have any questions, please use the [GitHub Discussions Q&A](https://github.com/jaqx0r/mtail/discussions/new?category=q-a).

We also have an email list : https://groups.google.com/forum/#!forum/mtail-users

For any defects please [file a new issue](https://github.com/jaqx0r/mtail/issues/new).
