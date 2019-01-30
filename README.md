<img src="https://raw.githubusercontent.com/google/mtail/master/logo.png" alt="mtail" title="mtail" align="right" width="140">

# mtail - extract whitebox monitoring data from application logs for collection into a timeseries database

[![GoDoc](https://godoc.org/github.com/google/mtail?status.png)](http://godoc.org/github.com/google/mtail)
[![CircleCI Build Status](https://circleci.com/gh/google/mtail.svg?style=shield&circle-token=:circle-token)](https://circleci.com/gh/google/mtail)
[![Coverage Status](https://coveralls.io/repos/github/google/mtail/badge.svg?branch=master)](https://coveralls.io/github/google/mtail?branch=master)
[![Go Report Card](https://goreportcard.com/badge/github.com/google/mtail)](https://goreportcard.com/report/github.com/google/mtail)

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
    counter line_count
    /$/ {
      line_count++
    }

Metrics are exported for scraping by a collector as JSON or Prometheus format
over HTTP, or can be periodically sent to a collectd, StatsD, or Graphite
collector socket.

Read the [programming guide](docs/Programming-Guide.md) if you want to learn how
to write mtail programs.

Mailing list: https://groups.google.com/forum/#!forum/mtail-users

## Installation

There are various ways of installing **mtail**.

### Precompiled binaries

Precompiled binaries for released versions are available in the
[Releases page](https://github.com/google/mtail/releases) on Github. Using the
latest production release binary is the recommended way of installing **mtail**.

Windows, OSX and Linux binaries are available.

### Building from source

To build `mtail` from the source code yourself you need to have a working Go
environment with version 1.9 or greater installed.  `mtail` is `go get`able and
`go install`able from this repository but is best if you use the Makefile to
build it.

```
go get -u github.com/google/mtail
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

`mtail` works best when it paired with a timeseries-based calculator and
alerting tool, like [Prometheus](http://prometheus.io).

> So what you do is you take the metrics from the log files and
> you bring them down to the monitoring system?

[It deals with the instrumentation so the engineers don't have
to!](http://www.imdb.com/title/tt0151804/quotes/qt0386890)  It has the
extraction skills!  It is good at dealing with log files!!

## Read More

Read more about `mtail` in the [Programming Guide](docs/Programming-Guide.md), [Language reference](docs/Language.md), instructions on [Building from source](docs/Building.md), help for [Interoperability](docs/Interoperability.md) with other monitoring system components, and [Deploying](docs/Deploying.md) and [Troubleshooting](docs/Troubleshooting.md) your programs and installation.


