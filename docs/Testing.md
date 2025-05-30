# Testing `mtail` programmes

## Introduction

By default any compile errors are logged to the standard log `/tmp/mtail.INFO`
unless otherwise redirected.  (You can emit to standard out with
`--logtostderr` flag.)  Program errors are also printed on the HTTP status
handler, by default at port 3903.

If you want more debugging information, `mtail` provides a few flags to assist with testing your program in standalone mode.

# Details

## Compilation errors

The `compile_only` flag will run the `mtail` compiler, print any error messages, and then exit.

You can use this to check your programs are syntactically valid during the development process.

```
mtail --compile_only --progs ./progs
```

This could be added as a pre-commit hook to your source code repository.

## Testing programs

The `one_shot` flag will compile and run the `mtail` programs, then feed in any
logs specified from the beginning of the file (instead of tailing them), then
print to the log all metrics collected.

You can use this to check that your programs are giving the expected output
against some gold standard log file samples.

```
mtail --one_shot --progs ./progs --logs testdata/foo.log
```

### Continuous Testing

If you wish, send a PR containing your program, some sample input, and a golden
output to be run as a test in
http://github.com/jaqx0r/mtail/blob/main/ex_test.go to ensure that mtail
never breaks your program (or that your program gets any updates if the
language changes.)

To have a syntax-only compile test, merely send in a PR with the program in the
examples directory.

The `TestExamplePrograms` behaves like the `one_shot` flag, and
`TestCompileExamplePrograms` tests that program syntax is correct.

# Test writing

Use the `testutil` module where possible.

Do not use time.Sleep; poll for events.  The `TestServer` provides a `PollWatched()` method for this purpose.  Even integration tests which write to disk can be fast and not require sleeps to synchronise.

Use the `if testing.Short()` signal in tests with disk access so that the `make smoke` command is fast.

Do not comment out tests, prefer to use the t.Skip() method indicating why it's not working if a test needs to be disabled.  This keeps them visible and compilable.

# Troubleshooting

For more information about debugging mtail programs, see the tips under [Troubleshooting](Troubleshooting.md)
