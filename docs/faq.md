# FAQ

"Frequently" is probably an overstatement, but here's a collection of questions and answers that pop up on the mailing list and issues.

## I don't like a particular label on the metrics.  How do I remove it?

All the labels are under your own control, except for the `prog` label which is used for namespace deconfliction -- i.e. multiple programs can be running in `mtail` and they should not be able to affect each other.

It is best if you do some post processing in your collection system and configure it to filter out the `prog` label, so that strange aggregations don't occur.

In Prometheus, this could be achieved like so:

```
metric_relabel_configs:
   - target_label: prog
     replacement: ''
```

(See [this comment](https://github.com/google/mtail/issues/59#issuecomment-303531070)).


## `mtail` isn't propagating the scraped timestamp to Prometheus

`mtail` lets you use the `settimestamp()` function to extract a timestamp from
a log file, and use that timestamp to carry to the monitoring system the
closest thing that `mtail` knows to be the actual time of the event, and not
the time at which `mtail` scraped the log.

However, Prometheus uses the existence of a timestamp to signal that a metric
can become stale, and so if, for example, a slow moving counter does not get
updated in some time window (5m by default) Prometheus will forget all about
it.

`mtail`, being a proxy for metrics, falls under bbrazil's comment on the
prometheus-users list, in which he says ["It doesn't make sense to have
timestamps for direct instrumentation, only for proxying metrics from another
monitoring system with a custom
collector."](https://groups.google.com/forum/#!msg/prometheus-users/qgxKH6_gYzM/LyO5wGO6BwAJ).

I consider the Prometheus behaviour broken, but to avoid any confusion,
`mtail` by default disables exporting timestamps to Prometheus.

You can turn this behaviour back on with the `--emit_metric_timestamp`
commandline flag, and if you have slow moving counters, you should tune your
Prometheus' `query.lookback-delta` parameter.  See also [Staleness under
Querying
Basics](https://prometheus.io/docs/prometheus/latest/querying/basics/#staleness)
in the Prometheus docs.

On the flipside, if you feel lie the latency between your application logging an event, and that event going into a log file, and mtail reading it, and processing it is small enough that you don't care:

    a. awesome! I'll take that as a compliment on `mtail`'s speed.
    b. you should remove any timestamp processing code from your programs to avoid that unnecessary work
    

## Why doesn't `mtail` persist variables and metric values between restarts?

`mtail` is intended to be stateless, deferring the problem of long term metric
storage to a timeseries database and collector like
[Prometheus](https://prometheus.io).

Partially this reason is technical -- not having to save checkpoints and restore them makes the program much simpler.

This means that mtail programs should prefer metrics that perform better in
stateless systems, like counters rather than gauges.  Prometheus for example is
capable of handling counter resets in its rate and delta calculations, so mtail
not remembering the value of a metric should not be cause for concern.

Another reason is that failure is normal, and thus Prometheus handles these
counter restarts because they are normal.  If `mtail` checkpointed its state,
filesystem and state file corruption will still occur, and in those edge cases
a counter reset would still be observed, and thus need to be handled
regardless.

So, given that the monitoring system needs to handle missing and resetting data
already in a distributed system, there is no compelling reason to implement
metric checkpointing in `mtail` as well.  It just adds complexity for little
overall gain.
