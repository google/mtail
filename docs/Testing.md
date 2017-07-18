# Introduction

mtail provides a few flags to assist with testing your program.

# Details

## Compilation errors

The `compile_only` flag will run the `mtail` compiler, print any error messages, and then exit.

You can use this to check your programs are syntactically valid during the development process.

```
mtail --compile_only --progs ./progs
```

## Testing programs

The `one_shot` flag will compile and run the `mtail` programs, then feed in any logs specified from the beginning of the file (instead of tailing them), then print out in JSON format all metrics collected.

You can use this to check that your programs are giving the expected output against some gold standard log file samples.

```
mtail --one_shot --progs ./progs --logs testdata/foo.log
```

# Troubleshooting

For more information about debugging mtail programs, see the tips under [Troubleshooting](Troubleshooting.md)
