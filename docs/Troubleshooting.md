# Troubleshooting `mtail` installations

This page gives an overview of some avenues to debug your `mtail` installation.

Also, see the [FAQ](faq.md).

## Reporting a problem

Please when reporting a problem, include the `mtail` version:

 * the output of `mtail --version`
 * the first lines of the INFO log (`/tmp/mtail.INFO` by default)
 * the top of the status page (on HTTP port 3903 by default)

## `go get` or build problems

### `package github.com/jaqx0r/mtail: no Go files`

You're using go 1.11 or higher, which now starts to use go modules, and doesn't like source code layouts like `mtail` which doesn't have any Go files in the top directory.

Either set `GO111MODULE=on` environment variable first, or `go get` the binary directly:

`go get github.com/jaqx0r/mtail/cmd/mtail`

vs

```
GO111MODULE=on go get -u github.com/jaqx0r/mtail
cd $GOPATH/src/github.com/jaqx0r/mtail
make install
```

## Compilation problems

Compilation problems will be emitted to the standard INFO log

 * which is visible either on stderr if `mtail` is run with the `--logtostderr` flag
 * which is stored in the location provided by the `--log_dir` flag (usually, /tmp)

(The behaviour of glog is documented in https://github.com/golang/glog)

Errors for the most recent version of the program will also be displayed on the
standard status page (served over HTTP at port 3903 by default) in the *Program Loader* section.

If a program fails to compile, it will not be loaded.  If an existing program
has been loaded, and a new version is written to disk (by you, or a
configuration management system) and that new version does not compile,
`mtail` will log the errors and not interrupt or restart the existing, older program.

The `--compile_only` flag will only attempt to compile the programs and not
execute them.  This can be used for pre-commit testing, for example.

### Syntax trees, type information, and virtual machine bytecode

More detailed compiler debugging can be retrieved by using the `--dump_ast`, `--dump_ast_types`, and `--dump_bytecode`, all of which dump their state to the INFO log.

For example, type errors logged such as
`prog.mtail: Runtime error: conversion of "-0.000000912" to int failed: strconv.ParseInt: parsing "-0.000000912": invalid syntax` suggest an invalid type inference of `int` instead of `float` for some program symbol or expression.  Use the `--dump_ast_types` flag to see the type annotated syntax tree of the program for more details.

When reporting a problem, please include the AST type dump.

## Memory or performance issues

`mtail` is a virtual machine emulator, and so strange performance issues can occur beyond the imagination of the author.

The standard Go profiling tool can help.  Start with a cpu profile:

`go tool pprof /path/to/mtail http://localhost:3903/debug/pprof/profile'

or a memory profile:

`go tool pprof /path/to/mtail http://localhost:3903/debug/pprof/heap'

There are many good guides on using the profiling tool:

 * https://software.intel.com/en-us/blogs/2014/05/10/debugging-performance-issues-in-go-programs is one such guide.


The goroutine stack dump can also help explain what is happening at the moment.

http://localhost:3903/debug/pprof/goroutine?debug=2 shows the full goroutine stack dump.

 * `(*Watcher).readEvents` reads events from the filesystem
 * `(*Tailer).run` processes log change events; `.read` reads the latest log lines
 * `(*Loader).processEvents` handles filesystem event changes regarding new program text
 * `(*Loader).processLines` handles new lines coming from the log tailer
 * `(*MtailServer).WaitForShutdown` waits for the other components to terminate
 * `(*Exporter).StartMetricPush` exists if there are any push collectors (e.g. Graphite) to push to
 * `(*Exporter).HandlePrometheusMetrics` exists if an existing Prometheus pull collection is going on

There is one `(*VM).Run` stack per program.  These are opaque to the goroutine
stack dump as they execute the bytecode.  However, the second argument to `Run`
on the stack is the first four letters of the program name, encoded as ASCII.
You can transcode these back to their names by doing a conversion from the
int32 value in hex provided in the stack, e.g.: 0x61706163 -> 'apac' (probably
an apache log program); 0x7273796e -> 'rsyn' (probably an rsyncd log program)

Obvious problems seen in the goroutine stack dump are long-waiting goroutines, usually on mutexes.
(they show their block time in minutes, e.g. `goroutine 38 [semacquire, 1580
minutes]:`) which usually also manifest as a logjam (no pun intended) in the
loader, tailer, and watcher goroutines (in state 'chan send').

## Distributed Tracing

`mtail` can export traces to the [Jaeger](https://www.jaegertracing.io/) trace collector.  Specify the Jaeger endpoint with the `--jaeger_endpoint` flag

```
mtail --jaeger_endpoint http://localhost:14268/api/traces
```

The `--trace_sample_period` flag can be used to set how often a trace is sampled and sent to the collector.  Set it to `100` to collect one in 100 traces.

## Deployment problems

The INFO log at `/tmp/mtail.INFO` by default contains lots of information about
any errors encountered.  Adding the `-v=2` flag raises the verbosity.  See the
[glog](https://github.com/golang/glog) manual for more logging flag options.

The `one_shot` and `logtostderr` flags may come in helpful for quickly
launching mtail in non-daemon mode in order to flush out deployment issues like
permissions problems.
