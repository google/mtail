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

