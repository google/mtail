# Contribution style guide

## Table tests

Use the `t.Run` subtest form.  This assists debugging by printing the name of
the table entry without additional parameters to t.Log and t.Error later on.
It also means that the `-run` and `-bench` flags can be used to filter a specific
test without excessive comment-and-rebuild cycles.

Prefer to construct the subtest's name from the test parameters with
`fmt.Sprintf`, otherwise use a `name` field.

When comparing results, use `deep.Equal`.  The parameter order should always be
`expected`, then `observed`.  This makes the diff output read like "the observed
value is not equal to the expected value."

If there is a non-nil diff result, emit it with `t.Error(diff)`.  If multiple
diffs are emitted in a single test, prefix the emission with a `t.Log` of the
name of the result variable or function under test.

