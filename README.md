<img src="https://raw.githubusercontent.com/google/mtail/main/logo.png" alt="mtail" title="mtail" align="right" width="140">

# mtail - extract internal monitoring data from application logs for collection into a timeseries database

[![ci](https://github.com/google/mtail/workflows/CI/badge.svg)](https://github.com/google/mtail/actions?query=workflow%3ACI+branch%3main)
[![GoDoc](https://godoc.org/github.com/google/mtail?status.png)](http://godoc.org/github.com/google/mtail)
[![Go Report Card](https://goreportcard.com/badge/github.com/google/mtail)](https://goreportcard.com/report/github.com/google/mtail)
[![OSS-Fuzz](https://oss-fuzz-build-logs.storage.googleapis.com/badges/mtail.svg)](https://bugs.chromium.org/p/oss-fuzz/issues/list?sort=-opened&can=1&q=proj:mtail)
[![codecov](https://codecov.io/gh/google/mtail/branch/main/graph/badge.svg)](https://codecov.io/gh/google/mtail)

`mtail` is a tool for extracting metrics from application logs to be exported
into a timeseries database or timeseries calculator for alerting and
dashboarding.

It fills a monitoring niche by being the glue between applications that do not
export their own internal state (other than via logs) and existing monitoring
systems, such that system operators do not need to patch those applications to
instrument them or writing custom extraction code for every such application.

The extraction is controlled by [mtail programs](docs/Programming-Guide.md)
which define patterns and actions:

    # simple line counter
    counter lines_total
    /$/ {
      lines_total++
    }

Metrics are exported for scraping by a collector as JSON or Prometheus format
over HTTP, or can be periodically sent to a collectd, StatsD, or Graphite
collector socket.

Read the [programming guide](docs/Programming-Guide.md) if you want to learn how
to write mtail programs.

Ask general questions on the users mailing list: https://groups.google.com/g/mtail-users

## Installation

There are various ways of installing **mtail**.

### Precompiled binaries

Precompiled binaries for released versions are available in the
[Releases page](https://github.com/google/mtail/releases) on Github. Using the
latest production release binary is the recommended way of installing **mtail**.

Windows, OSX and Linux binaries are available.

### Building from source

The simplest way to get `mtail` is to `go get` it directly.

`go get github.com/google/mtail/cmd/mtail`

This assumes you have a working Go environment with a recent Go version.  Usually mtail is tested to work with the last two minor versions  (e.g. Go 1.12 and Go 1.11).

If you want to fetch everything, you need to turn on Go Modules to succeed because of the way Go Modules have changed the way go get treats source trees with no Go code at the top level.

```
GO111MODULE=on go get -u github.com/google/mtail
cd $GOPATH/src/github.com/google/mtail
make install
```

If you develop the compiler you will need some additional tools
like `goyacc` to be able to rebuild the parser.

See the [Build instructions](docs/Building.md) for more details.

A `Dockerfile` is included in this repository for local development as an
alternative to installing Go in your environment, and takes care of all the
build dependency installation, if you don't care for that.


## Deployment

`mtail` works best when paired with a timeseries-based calculator and
alerting tool, like [Prometheus](http://prometheus.io).

### Source-to-Program Mapping

By default, mtail processes every log line with every loaded program. For large installations with many logs and programs, this can be inefficient. You can optimize performance by mapping specific log sources to specific programs using the `--source_mapping_file` option:

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

## Read More

Full documentation at http://google.github.io/mtail/

Read more about writing `mtail` programs:

* [Programming Guide](docs/Programming-Guide.md)
* [Language Reference](docs/Language.md)
* [Metrics](docs/Metrics.md)
* [Managing internal state](docs/state.md)
* [Testing your programs](docs/Testing.md)

Read more about hacking on `mtail`

* [Building from source](docs/Building.md)
* [Contributing](CONTRIBUTING.md)
* [Style](docs/style.md)

Read more about deploying `mtail` and your programs in a monitoring environment

* [Deploying](docs/Deploying.md)
* [Interoperability](docs/Interoperability.md) with other systems
* [Troubleshooting](docs/Troubleshooting.md)
* [FAQ](docs/faq.md)


## Getting more help and reporting defects

If you have any questions, please use the [GitHub Discussions Q&A](https://github.com/google/mtail/discussions/new?category=q-a).

We also have an email list : https://groups.google.com/forum/#!forum/mtail-users

For any defects please [file a new issue](https://github.com/google/mtail/issues/new).
