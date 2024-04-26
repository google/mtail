# `mtail` Programming Guide

## Introduction

`mtail` is very simple and thus limits what is possible with metric
manipulation, but is very good for getting values into the metrics. This page
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

`mtail` attributes a timestamp to each event.

If no timestamp exists in the log and none explicitly parsed by the mtail
program, then mtail will use the current system time as the time of the event.

Many log files include the timestamp of the event as reported by the logging
program. To parse the timestamp, use the `strptime` function with a
[Go time.Parse layout string](https://golang.org/pkg/time/#Parse).

```
/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)\s+[\w\.-]+\s+sftp-server/ {
    strptime($date, "Jan _2 15:04:05")
```

Don't try to disassemble timestamps into component parts (e.g. year, month, day)
separately. Keep them in the same format as the log file presents them and
change the strptime format string to match it.

```
/^/ +
/(?P<date>\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}) / +
/.*/ +
/$/ {
    strptime($date, "2006/01/02 15:04:05")
```

N.B. If no timestamp parsing is done, then the reported timestamp of the event
may add some latency to the measurement of when the event really occurred.
Between your program logging the event, and mtail reading it, there are many
moving parts: the log writer, some system calls perhaps, some disk IO, some more
system calls, some more disk IO, and then mtail's virtual machine execution.
While normally negligible, it is worth stating in case users notice offsets in
time between what mtail reports and the event really occurring. For this reason,
it's recommended to always use the log file's timestamp if one is available.

## Repeating common timestamp parsing

The decorator syntax was designed with common timestamp parsing in mind. It
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
            strptime($legacy_date, "Jan _2 15:04:05")
        }
        # If the RFC3339 style matched, parse it this way.
        len($rfc3339_date) > 0 {
            strptime($rfc3339_date, "2006-01-02T15:04:05-07:00")
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
```

Both the foo and bar pattern actions will have the syslog timestamp parsed from
them before being called.

### Timestamps with strange characters in them

Go's [time.Parse](https://golang.org/pkg/time/#Parse) does not like underscores
in the format string, which may happen when one is attempting to parse a
timestamp that does have underscores in the format. Go treats the underscore as
placeholding an optional digit.

To work around this, you can use `subst()` to rewrite the timestamp before
parsing:

```
/(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2}) / {
  strptime(subst("_", " ", $1), "2006-01-02 15:04:05")
}
```

Note the position of the underscore in the regular expression match.

## Conditional structures

The `/pattern/ { action }` idiom is the normal conditional control flow
structure in `mtail` programs.

If the pattern matches, then the actions in the block are executed. If the
pattern does not match, the block is skipped.

The `else` keyword allows the program to perform action if the pattern does not
match.

```
/pattern/ {
  action
} else {
  alternative
}
```

The example above would execute the "alternative" block if the pattern did not
match the current line.

The `otherwise` keyword can be used to create control flow structure reminiscent
of the C `switch` statement. In a containing block, the `otherwise` keyword
indicates that this block should be executed only if no other pattern in the
same scope has matched.

```
{
/pattern1/ { _action1_ }
/pattern2/ { _action2_ }
otherwise { _action3_ }
}
```

In this example, "action3" would execute if both pattern1 and pattern2 did not
match the current line.

### Explicit matching

The above `/pattern/ { _action_ }` form implicitly matches the current input log
line.

If one wants to match against another string variable, one can use the `=~`
operator, or to negate the match the `!~`, like so:

```mtail
  $1 =~ /GET/ {
    ...
  }
```

## Storing intermediate state

Hidden metrics are metrics that can be used for internal state and are never
exported outside of `mtail`. For example if the time between pairs of log lines
needs to be computed, then a hidden metric can be used to record the timestamp
of the start of the pair.

**Note** that the `timestamp` builtin *requires* that the program has set a log
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
`connection_time` keyed by the "pid" of the connection. Later when the
connection end is logged, the delta between the current log timestamp and the
start timestamp is computed and added to the total connection time.

In this example, the average connection time can be computed in a collection
system by taking the ratio of the number of connections (`connections_total`)
over the time spent (`connection_time_total`). For example in
[Prometheus](http://prometheus.io) one might write:

```
connection_time_10s_moving_avg =
  rate(connections_total[10s])
    / on job
  rate(connection_time_total[10s])
```

Note also that the `del` keyword is used to signal to `mtail` that the
connection_time value is no longer needed. This will cause `mtail` to delete the
datum referenced by that label from this metric, keeping `mtail`'s memory usage
under control and speeding up labelset search time (by reducing the search
space!)

Alternatively, the statement `del connection_time[$pid] after 72h` would do the
same, but only if `connection_time[$pid]` is not changed for 72 hours. This form
is more convenient when the connection close event is lossy or difficult to
determine.

See [state](state.md) for more information.

## Computing moving averages

`mtail` deliberately does not implement complex mathematical functions. It wants
to process a log line as fast as it can. Many other products on the market
already do complex mathematical functions on timeseries data, like
[Prometheus](http://prometheus.io) and [Riemann](http://riemann.io), so `mtail`
defers that responsibility to them. (Do One Thing, and Do It Pretty Good.)

But say you still want to do a moving average in `mtail`. First note that
`mtail` has no history available, only point in time data. You can update an
average with a weighting to make it an exponential moving average (EMA).

```
gauge average

/some (\d+) match/ {
  # Use a smoothing constant 2/(N + 1) to make the average over the last N observations
  average = 0.9 * $1 + 0.1 * average
}
```

However this doesn't take into account the likely situation that the matches
arrive irregularly (the time interval between them is not constant.)
Unfortunately the formula for this requires the exp() function (`e^N`) as
described here:
http://stackoverflow.com/questions/1023860/exponential-moving-average-sampled-at-varying-times
. I recommend you defer this computation to the collection system

## Histograms

Histograms are preferred over averages in many monitoring howtos, blogs, talks,
and rants, in order to give the operators better visibility into the behaviour
of a system.

`mtail` supports histograms as a first class metric kind, and should be created
with a list of bucket boundaries:

```
histogram foo buckets 1, 2, 4, 8
```

creates a new histogram `foo` with buckets for ranges [0-1), [1-2), [2-4),
[4-8), and from 8 to positive infinity.

> *NOTE: The 0-n and m-+Inf buckets are created automatically.*

You can put labels on a histogram as well: `histogram
apache_http_request_time_seconds buckets 0.005, 0.01, 0.025, 0.05 by
server_port, handler, request_method, request_status, request_protocol`

At the moment all bucket boundaries (excepting 0 and positive infinity) need to
be explicitly named (there is no shorthand form to create geometric
progressions).

Assignment to the histogram records the observation: `### # HTTP Requests with
histogram buckets. #
apache_http_request_time_seconds[$server_port][$handler][$request_method][$request_status][$request_protocol] =
$time_us / 1000000`

In tools like [Prometheus](http://prometheus.io) these can be manipulated in
aggregate for computing percentiles of response latency.

```
apache_http_request_time:rate10s = rate(apache_http_request_time_seconds_bucket[10s])
apache_http_request_time_count:rate10s = rate(apache_http_request_time_seconds_count[10s])


apache_http_request_time:percentiles =
  apache_http_request_time:rate10s
    / on (job, port, handler, request_method, request_status, request_protocol)
  apache_http_request_time_seconds_count:rate10s
```

This new timeseries can be plotted to see the percentile bands of each bucket,
for example to visualise the distribution of requests moving between buckets as
the performance of the server changes.

Further, these timeseries can be used for
[Service Level](https://landing.google.com/sre/book/chapters/service-level-objectives.html)-based
alerting (a technique for declaring what a defensible service level is based on
the relative costs of engineering more reliability versus incident response,
maintenance costs, and other factors), as we can now see what percentage of
responses fall within and without a predefined service level:

```
apache_http_request_time:latency_sli =
  apache_http_request_time:rate10s{le="200"}
    / on (job, port, handler, request_method, request_status, request_protocol)
  apache_http_request_time_seconds_count:rate10s

ALERT LatencyTooHigh
IF apache_http_request_time:latency_sli < 0.555555555
LABELS { severity="page" }
ANNOTATIONS {
  summary = "Latency is missing the service level objective"
  description = "Latency service level indicator is {{ $value }}, which is below nine fives SLO."
}
```

In this example, prometheus computes a service level indicator of the ratio of
requests at or below the target of 200ms against the total count, and then fires
an alert if the indicator drops below nine fives.

## Parsing number fields that are sometimes not numbers

Some logs, for example Varnish and Apache access logs, use a hyphen rather than
a zero.

You may be tempted to use a programme like

```
counter total

/^[a-z]+ ((?P<response_size>\d+)|-)$/ {
  $response_size > 0 {
    total = $response_size
  }
}
```

to parse a log like

```
a 99
b -
```

except that `mtail` will issue a runtime error on the second line like `Runtime
error: strconv.ParseInt: parsing "": invalid syntax`.

This is because in this programme the capture group is only matching on a set of
digits, and is not defined when the alternate group matches (i.e. the hyphen).

Instead one can test the value of the surrounding capture group and do nothing
if the value matches a hyphen:

```
counter total

/^[a-z]+ ((?P<response_size>\d+)|-)$/ {
  $1 != "-" {
    total = $response_size
  }
}
```

`mtail` does not presently have a way to test if a capture group is defined or
not.

## Parsing numbers with extra characters

Some logs contain human readable numbers, inserting thousands-separators (comma
or full stop depending on your locale.) You can remove them with the `subst`
function:

```
/sent (?P<sent>[\d,]+) bytes  received (?P<received>[\d,]+) bytes/ {
    # Sum total bytes across all sessions for this process
    bytes_total["sent"] += int(subst(",", "", $sent))
    bytes_total["received"] += int(subst(",", "", $received))
}
```

As `subst` is of type String, the type inference will assign a Text type to
bytes total, so here we must explicitly instruct `mtail` that we are expecting
this to be an Int by using the `int` cast function.

# Avoiding unnecessary work

You can stop the program if it's fed data from a log file you know you want to
ignore:

```
getfilename() !~ /apache.access.?log/ {
  stop
}
```

This will check to see if the input filename looks like the regular expression
pattern `apache.access.?log`, i.e. matching `/var/log/apache/accesslog`, and not
attempt any further pattern matching on the log line if it doesn't.

# Canonicalising keys

Some logs like webserver logs describe common elements with unique identifiers
in them, which can result in lots of metric keys and no useful count if left
alone. To rewrite these capture groups, use `subst()` with a pattern as the
first argument:

```mtail
hidden text route
counter http_requests_total by method, route

/(?P<method\S+) (?P<url>\S+)/ {
  route = subst(/\/d+/, "/:num", $url)
  http_requests_total[method][route]++
}
```

Here we replace any number part following a `/` in the `$url` capture group with
the literal string `/:num`, so we end up counting only the static part of a URL
route.
