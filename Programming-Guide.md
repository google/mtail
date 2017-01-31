# Introduction

`mtail` is very simple and thus limits what is possible with metric
manipulation, but is very good for getting values into the metrics.  This page
describes some common patterns for writing useful `mtail` programs.


## Changing the exported variable name

`mtail` only lets you use "C"-style identifier names in the program text, but
you can rename the exported variable as it gets presented to the collection
system if you don't like that.

```
counter connection_time_total as "connection-time_total"
```


## Reusing pattern pieces

If the same pattern gets used over and over, then define a constant and avoid
having to check the spelling of every occurrence.

```
# Define some pattern constants for reuse in the patterns below.
const IP /\d+(\.\d+){3}/
const MATCH_IP /(?P<ip>/ + IP + /)/

...

    # Duplicate lease
    /uid lease / + MATCH_IP + / for client .* is duplicate on / {
        duplicate_lease++
    }
```

## Parse the log line timestamp

`mtail` requires a timestamp to attribute to each event.  It assumes that the
log files being processed have stamped each line of input with a timestamp.
Use the `strptime` function with
a [Go time.Parse layout string](https://golang.org/pkg/time/#Parse).

```
/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)\s+[\w\.-]+\s+sftp-server/ {
    strptime($date, "Jan _2 15:04:05")
```

## Common timestamp parsing

The decorator syntax was designed with common timestamp parsing in mind.  It
allows the code for getting the timestamp out of the log line to be reused and
make the rest of the program text more readable and thus maintainable.

```
# The `syslog' decorator defines a procedure.  When a block of mtail code is
# "decorated", it is called before entering the block.  The block is entered
# when the keyword `next' is reached.
def syslog {
    /(?P<date>(?P<legacy_date>\w+\s+\d+\s+\d+:\d+:\d+)|(?P<rfc3339_date>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d+[+-]\d{2}:\d{2}))/ +
        /\s+(?:\w+@)?(?P<hostname>[\w\.-]+)\s+(?P<application>[\w\.-]+)(?:\[(?P<pid>\d+)\])?:\s+(?P<message>.*)/ {
        # If the legacy_date regexp matched, try this format.
        len($legacy_date) > 0 {
            strptime($2, "Jan _2 15:04:05")
        }
        # If the RFC3339 style matched, parse it this way.
        len($rfc3339_date) > 0 {
            strptime($rfc3339_date, "2006-01-02T03:04:05-0700")
        }
        # Call into the decorated block
        next
    }
}
```

This can be used around any blocks later in the program.

```
@syslog {
/foo/ {
  ...
}

/bar/ {
}
} # end @syslog decorator

## Conditional structures

The `/pattern/ { action }` idiom is the normal conditional control flow structure in `mtail` programs.

If the pattern matches, then the actions in the block are executed.  If the
pattern does not match, the block is skipped.

The `else` keyword allows the program to perform action if the pattern does not match.

```
/pattern/ {
  action
} else {
  alternative
}
```

The example above would execute the "alternative" block if the pattern did not
match the current line.

The `otherwise` keyword can be used to create control flow structure
reminiscent of the C `switch` statement.  In a containing block, the
`otherwise` keyword indicates that this block should be executed only if no
other pattern in the same scope has matched.

```
{
/pattern1/ { _action1_ }
/pattern2/ { _action2_ }
otherwise { _action3_ }
}
```

In this example, "action3" would execute if both pattern1 and pattern2 did not
match the current line.

## Storing intermediate state

Hidden metrics are metrics that can be used for internal state and are never
exported outside of `mtail`.  For example if the time between pairs of log
lines needs to be computed, then a hidden metric can be used to record the
timestamp of the start of the pair.

**Note** that the `timestamp` builtin _requires_ that the program has set a log
line timestamp with `strptime` or `settime` before it is called.

```
hidden gauge connection_time by pid
...

  # Connection starts
  /connect from \S+ \(\d+\.\d+\.\d+\.\d+\)/ {
    connections_total++

    # Record the start time of the connection, using the log timestamp.
    connection_time[$pid] = timestamp()
  }

...

  # Connection summary when session closed
  /sent (?P<sent>\d+) bytes  received (?P<received>\d+) bytes  total size \d+/ {
    # Sum total bytes across all sessions for this process
    bytes_total["sent"] += $sent
    bytes_total["received"] += $received
    
    # Count total time spent with connections open, according to the log timestamp.
    connection_time_total += timestamp() - connection_time[$pid]

    # Delete the datum referenced in this dimensional metric.  We assume that
    # this will never happen again, and hint to the VM that we can garbage
    # collect the memory used.
    del connection_time[$pid]
  }
```

In this example, the connection timestamp is recorded in the hidden variable
`connection_time` keyed by the "pid" of the connection.  Later when the
connection end is logged, the delta between the current log timestamp and the
start timestamp is computed and added to the total connection time.

In this example, the average connection time can be computed in a collection
system by taking the ratio of the number of connections (`connections_total`)
over the time spent (`connection_time_total`).  For example
in [Prometheus](http://prometheus.io) one might write:

```
connection_time_10s_moving_avg = 
  rate(connections_total[10s])
    / on job
  rate(connection_time_total[10s])
```

Note also that the `del` keyword is used to signal to `mtail` that the
connection_time value is no longer needed.  This will cause `mtail` to delete
the datum referenced by that label from this metric, keeping `mtail`'s memory
usage under control and speeding up labelset search time (by reducing the
search space!)

## Computing moving averages

`mtail` deliberately does not implement complex mathematical functions.  It
wants to process a log line as fast as it can.  Many other products on the
market already do complex mathematical functions on timeseries data,
like [Prometheus](http://prometheus.io) and [Riemann](http://riemann.io), so
`mtail` defers that responsibility to them.  (Do One Thing, and Do It Pretty
Good.)

But say you still want to do a moving average in `mtail`.  First note that
`mtail` has no history available, only point in time data.  You can update an
average with a weighting to make it an exponential moving average (EMA).

```
gauge average

/some (\d+) match/ {
  # Use a smoothing constant 2/(N + 1) to make the average over the last N observations
  average = 0.9 * $1 + 0.1 * average
}
```

## Histograms

Histograms are preferred over averages in many monitoring howtos, blogs, talks,
and rants, in order to give the operators better visibility into the behaviour
of a system.

At the moment, `mtail` does not have first class support for a distribution
type, but a histogram can be easily created by making one label on a
dimensioned metric the name of the histogram bucket.

```
counter apache_http_request_time_microseconds by le, server_port, handler, request_method, request_status, request_protocol

...
  ###
  # HTTP Requests with histogram buckets.
  #
  apache_http_request_time_microseconds_count[$server_port][$handler][$request_method][$request_status][$request_protocol]++

  # These statements "fall through", so the histogram is cumulative.  The
  # collecting system can compute the percentile bands by taking the ratio of
  # each bucket value over the final bucket.

  # 5ms bucket.
  $time_us < 5000 {
    apache_http_request_time_microseconds["5000"][$server_port][$handler][$request_method][$request_status][$request_protocol]++
  }

  # 10ms bucket.
  $time_us < 10000 {
    apache_http_request_time_microseconds["10000"][$server_port][$handler][$request_method][$request_status][$request_protocol]++
  }

  # 25ms bucket.
  $time_us < 25000 {
    apache_http_request_time_microseconds["25000"][$server_port][$handler][$request_method][$request_status][$request_protocol]++
  }

  # 50ms bucket.
  $time_us < 50000 {
    apache_http_request_time_microseconds["50000"][$server_port][$handler][$request_method][$request_status][$request_protocol]++
  }

...

  # 10s bucket.
  $time_us < 10000000 {
    apache_http_request_time_microseconds["10000000"][$server_port][$handler][$request_method][$request_status][$request_protocol]++
  }

```

This example creates a histogram with a bucket label "le" that contains a count
of all requests that were "less than" the bucket label's value.

In tools like [Prometheus](http://prometheus.io) these can be manipulated in
aggregate for computing percentiles of response latency.

```
apache_http_request_time:rate10s = rate(apache_http_request_time_microseconds[10s])
apache_http_request_time_count:rate10s = rate(apache_http_request_time_microseconds_count[10s])


apache_http_request_time:percentiles = 
  apache_http_request_time:rate10s
    / on (job, port, handler, request_method, request_status, request_protocol)
  apache_http_request_time_microseconds_count:rate10s
```

This new timeseries can be plotted to see the percentile bands of each bucket,
for example to visualise the distribution of requests moving between buckets as
the performance of the server changes.

Further, these timeseries can be used
for
[Service Level](https://landing.google.com/sre/book/chapters/service-level-objectives.html)-based
alerting (a technique for declaring what a defensible service level is based on
the relative costs of engineering more reliability versus incident response,
maintenance costs, and other factors), as we can now see what percentage of
responses fall within and without a predefined service level:

```
apache_http_request_time:latency_sli = 
  apache_http_request_time:rate10s{le="200"}
    / on (job, port, handler, request_method, request_status, request_protocol)
  apache_http_request_time_microseconds_count:rate10s

ALERT LatencyTooHigh
IF apache_http_request_time:latency_sli < 0.555555555
LABELS { severity="page" }
ANNOTATIONS {
  summary = "Latency is missing the service level objective"
  description = "Latency service level indicator is {{ $value }}, which is below nine fives SLO."
}
```

In this example, prometheus computes a service level indicator of the ratio of
requests at or below the target of 200ms against the total count, and then
fires an alert if the indicator drops below five nines.

