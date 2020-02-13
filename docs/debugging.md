# Debugging mtail


## Parser bugs

Run a test with logtostderr and mtailDebug up to 3.

```
go test -run TestParserRoundTrip/decrement_counter --logtostderr --mtailDebug=3
```

`mtailDebug` at 2 dumps the parser states being traversed, and 3 includes the lexer token stream as well.

You can use this to improve error messages in the `%error` section of [`parser.y`](../internal/vm/parser/parser.y), if you compare the "error pops" messages with the state machine in the generated [`y.output`](../internal/vm/parser/y.output).

```
...
state-14 saw LSQUARE
error recovery pops state 14
error recovery pops state 106
error recovery pops state 49
error recovery pops state 14
error recovery pops state 2
error recovery pops state 0
```

Walking backwards from state 0 (`$start`), we can get a list of nonterminal names to put in the state machine match expression used in the `%error` directive, and fill in the gaps with our knowledge of the intermediate states in our parse tree.

```
%error stmt_list stmt conditional_statement logical_expr compound_statement conditional_statement logical_expr LSQUARE : "unexpected indexing of an expression"
```


## Fuzzer crashes

Build the fuzzer locally with clang and libfuzzer:

```
make vm-fuzzer CXX=clang CXXFLAGS=-fsanitize=fuzzer,address LIB_FUZZING_ENGINE=
```

Then we can run the fuzzer with our example crash; make sure it has no weird characters because the upstream fuzz executor doesn't shell-escape arguments.

```
./vm-fuzzer crash.mtail
```

If the crash is big, we can try to minimise it:

```
make fuzz-min CRASH=crash.mtail
```

Sometimes the minimiser will hit a local minima, but still look big; for example it doesn't know how to shrink variable names.

We can reformat the crash with [`cmd/mfmt`](../cmd/mfmt/main.go):

```
make mfmt
./mfmt --prog crash.mtail --write
```

so it's easier to read -- it'll be bigger cos of the whitespace and the minimiser should shrink it back to original size if everything is working well.

The formatted mtail program should help make it obvious what's happening and let you manually attempt to rename or remove parts of the program yourself -- perhaps a whole variable declaration and usage doesn't need to exist, but the minimiser will take a long time to figure that out.

Once we have the smallest program we can add it to the crash corpus in [`internal/vm/fuzz/`](../internal/vm/fuzz/) and running `make fuzz` should run and fail on it straight away.

Or, variants of the program can be added to the various `*Invalid` tests in parts of the `vm` module, e.g. [`parser_test.go`](../internal/vm/parser/parser_test.go) or [`checker_test.go`](../internal/vm/checker/checker_test.go) depending on where in the compiler the defect is occuring.

