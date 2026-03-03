# Deploying `mtail`

## Introduction

mtail is intended to run one per machine, and serve as monitoring glue for
multiple applications running on that machine. It runs one or more programs in a
1:1 mapping to those client applications.

## Configuration Overview

mtail is configured with commandline flags.

The `--help` flag will print a list of flags for configuring `mtail`.

(Flags may be prefixed with either `-` or `--`)

## Quickstart

Basic flags necessary to start `mtail`:

*   `--logs` is a comma separated list of filenames to extract from, but can
    also be used multiple times, and each filename can be a
    [glob pattern](http://godoc.org/path/filepath#Match). Named pipes can be
    read from when passed as a filename to this flag.
*   `--progs` is a directory path containing [mtail programs](Language.md).
    Programs must have the `.mtail` suffix.

mtail runs an HTTP server on port 3903, which can be changed with the `--port`
flag.

# Details

## Launching mtail

```
mtail --progs /etc/mtail --logs /var/log/syslog --logs /var/log/ntp/peerstats
```

`mtail` will start to read the specified logs from their current end-of-file,
and read new updates appended to these logs as they arrive. It will attempt to
correctly handle log files that have been rotated by renaming or symlink
changes.

### Getting the logs in

Use `--logs` multiple times to pass in glob patterns that match the logs you
want to tail. This includes named pipes.

A special file called `-` can be passed to read from standard input when it's a
pipe input.

```shell
/usr/bin/journalctl -o cat -f -n 0 | mtail -progs progs -logs -
```

N.B. Piped file input won't work. `mtail -logs - < file` is not supported.

### Polling the file system

`mtail` polls matched log files every `--poll_log_interval`, or 250ms by
default, the supplied `--logs` patterns for newly created or deleted log
pathnames.

Known and active logs are read until EOF every `--poll_interval`, or 250ms by
default.

Example: `mtail --progs /etc/mtail --logs /var/log/syslog --poll_interval 250ms
--poll_log_interval 250ms`

### Consume data in Apache Kafka

Use `--logs` flag to read data from Apache Kafka.

You need to convert kafka configuration into URL format.

You can refer to [parseKafkaURL](../internal//tailer/logstream/kafka.go#L26) function to write URL.

Example: `mtail --progs /etc/mtail --logs "kafka://test-group@localhost:9092/test-topic"`

### Setting garbage collection intervals

`mtail` accumulates metrics and log files during its operation. By default,
*every hour* both a garbage collection pass occurs looking for expired metrics,
and stale log files.

An expired metric is any metric that hasn't been updated in a time specified by
a `del after` form in a program.

A stale log file is any log being watched that hasn't been read from in 24
hours.

The interval between garbage collection runs can be changed on the commandline
with the `--expired_metrics_gc_interval` and `--stale_log_gc_interval` flags,
which accept a time duration string compatible with the Go
[time.ParseDuration](https://golang.org/pkg/time/#ParseDuration) function.

### Runtime error log rate

If your programs deliberately fail to parse some log lines then you may end up
generating lots of runtime errors which are normally logged at the standard INFO
level, which can fill your disk.

You can disable this with `--novm_logs_runtime_errors` or
`--vm_logs_runtime_errors=false` on the commandline, and then you will only be
able to see the most recent runtime error in the HTTP status console.

### Launching under Docker

`mtail` can be run as a sidecar process if you expose an application container's
logs with a volume.

`docker run -d --name myapp -v /var/log/myapp myapp`

for example exports a volume called `/var/log/myapp` (named the same as the
hypothetical path where `myapp`s logs are written.

Then launch the `mtail` docker image and pass in the volume:

```
docker run -dP \
   --name myapp-mtail \
   --volumes-from myapp \
   -v examples:/etc/mtail \
   mtail --logs /var/log/myapp --progs /etc/mtail
```

This example fetches the volumes from the `myapp` container, and mounts them in
the mtail container (which we've called `myapp-mtail`). We also mount the
`examples` directory as `/etc/mtail` in the container. We launch `mtail` with
the `logs` and `progs` flags to point to our two mounted volumes.

The `-P` flag ensures `mtail-myapp`'s port 3903 is exposed for collection, refer
to `docker ps` to find out where it's mapped to on the host.

## Writing the programme

Read the [Programming Guide](Programming-Guide.md) for instructions on how to
write an `mtail` program.

### Reloading programmes

`mtail` does not automatically reload programmes after it starts up. To ask
`mtail` to scan for and reload programmes from the supplied `--progs` directory,
send it a `SIGHUP` signal on UNIX-like systems.

For example, if configs are being delivered by a configuration management tool
like Puppet, then program Puppet to send a SIGHUP when it has copied a new
config file over.

```puppet
exec { 'reload_mtail_programmes':
  command => "killall -HUP mtail",
  refreshonly = True,
}

file { mtail_programme:
  source => mtail_programme,
  notify => Exec['reload_mtail_programmes'],
}
```

Alternatively, if you're using `scp` or some similar method to copy the
programme files without a receiver, then either follow it with a `ssh $host
'killall -HUP mtail'` or use a tool like
[`inotifywait`](https://linux.die.net/man/1/inotifywait) in a side process next
to mtail to watch for changes and send the reload signal.

```shell
inotifywait -m /etc/mtail/progs | while read event; do killall -HUP mtail; done
```

## Getting the Metrics Out

### Pull based collection

Point your collection tool at `localhost:3903/json` for JSON format metrics.

Prometheus can be directed to the /metrics endpoint for Prometheus text-based
format.

### Changing the listen address

The default port is `3903`, and can be changed with the `--port` commandline
flag.

The default address is `""` on the TCP protocol, which means it will bind to all
IP addresses on the system. This can be changed with the `--address` commandline
flag.

```
mtail --address=127.0.0.1 --port=8080`
```

Depending on your version of Go, the address "0.0.0.0" is treated by Go as
dual-stack; see https://github.com/golang/go/issues/17615 and
https://pkg.go.dev/net#Listen

### Push based collection

Use the `collectd_socketpath` or `graphite_host_port` flags to enable pushing to
a collectd or graphite instance.

Configure collectd on the same machine to use the unixsock plugin, and set
`collectd_socketpath` to that unix socket.

```
mtail --progs /etc/mtail --logs /var/log/syslog,/var/log/rsyncd.log --collectd_socketpath=/var/run/collectd-unixsock
```

Set `graphite_host_port` to be the host:port of the carbon server.

```
mtail --progs /etc/mtail --logs /var/log/syslog,/var/log/rsyncd.log --graphite_host_port=localhost:9999
```

Likewise, set `statsd_hostport` to the host:port of the statsd server.

Additionally, the flag `metric_push_interval_seconds` can be used to configure
the push frequency. It defaults to 60, i.e. a push every minute.

## Setting a default timezone

The `--override_timezone` flag sets the timezone that `mtail` uses for timestamp
conversion. By default, `mtail` assumes timestamps are in UTC.

To use the machine's local timezone, `--override_timezone=Local` can be used.

## Troubleshooting

Lots of state is logged to the log file, by default in `/tmp/mtail.INFO`. See
[Troubleshooting](Troubleshooting.md) for more information.

N.B. Oneshot mode (the `one_shot` flag on the commandline) can be used to check
that a program is correctly reading metrics from a log, but with the following
caveats:

*   Unlike normal operations, oneshot mode will read the logs from the start of
    the file to the end, then close them -- it does not continuously tail the
    file
*   The metrics will be printed to standard out when the logs are finished being
    read from.
*   mtail will exit after the metrics are printed out.

This mode is useful for debugging the behaviour of `mtail` programs and possibly
for permissions checking.
