# Tips for debugging `mtail`


## Parser bugs

Run a test with logtostderr and mtailDebug up to 3, and parser_test_debug enabled to see any AST results.

```
go test -run TestParserRoundTrip/decrement_counter --logtostderr --mtailDebug=3 --parser_test_debug
```

`mtailDebug` at 2 dumps the parser states being traversed, and 3 includes the lexer token stream as well.

## Improving parser syntax error messages

You can use this to improve error messages in the `%error` section of [`parser.y`](../internal/runtime/compiler/parser/parser.y), if you compare the "error recovery pops" messages with the state machine in the generated [`y.output`](../internal/runtime/compiler/parser/y.output).


```
go generate && go test -run TestParseInvalidPrograms/statement_with_no_effect --logtostderr --mtailDebug=3  --parser_test_debug
```

error log from test:
```
...
state-14 saw LSQUARE
error recovery pops state 14
error recovery pops state 102
error recovery pops state 46
error recovery pops state 14
error recovery pops state 2
error recovery pops state 0
```

This log says the lexer sent a LSQUARE token, and the parser was in state 14 when it saw it.  The snippet below from `y.output` indicates state 14 is never expecting a LSQUARE, and the following lines in the log above show the state stack being popped -- 0, 2, 14, 49, 102, 14.

Walking backwards from state 0 (`$start`), we can get a list of nonterminal names to put in the state machine match expression used in the `%error` directive, and fill in the gaps with our knowledge of the intermediate states in our parse tree.

`y.output`:
```
state 14
	conditional_statement:  logical_expr.compound_statement ELSE compound_statement 
	conditional_statement:  logical_expr.compound_statement 
	logical_expr:  logical_expr.logical_op opt_nl bitwise_expr 

	AND  shift 47
	OR  shift 48
	MATCH  shift 49
	NOT_MATCH  shift 50
	LCURLY  shift 46
	.  error

	compound_statement  goto 44
	logical_op  goto 45
```

State 14 to state 46 shifts a LCURLY operator, follow state 46 and we will find ourselves in `compound_statement`.

Add to `parser.y` the names of the states that ended up at the unexpected token, followed by the error message:
```
%error stmt_list stmt conditional_statement logical_expr compound_statement conditional_statement logical_expr LSQUARE : "unexpected indexing of an expression"
```

and instead of "syntax error", the parser now emits "unexpected indexing of an expression".


## Fuzzer crashes

Build the fuzzer locally with clang and libfuzzer:

```
make vm-fuzzer fuzz CXX=clang CXXFLAGS=-fsanitize=fuzzer,address LIB_FUZZING_ENGINE=
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

Once we have the smallest program we can add it to the crash corpus in [`internal/runtime/fuzz/`](../internal/runtime/fuzz/) and running `make fuzz` should run and fail on it straight away.

Or, variants of the program can be added to the various `*Invalid` tests in parts of the `vm` module, e.g. [`parser_test.go`](../internal/runtime/compiler/parser/parser_test.go) or [`checker_test.go`](../internal/runtime/compiler/checker/checker_test.go) depending on where in the compiler the defect is occurring.

If the crash is in `vm.go` then we can dump the program to see what AST and types, and bytecode it generates.

```
make mtail
./mtail --logtostderr --dump_ast_types --dump_bytecode --mtailDebug=3 --compile_only --progs crash.mtail
```


### Fuzzer crashes, part 2

Run the fuzz-repro target with the CRASH variable set, it'll do all of the above:

```
make fuzz-repro CRASH=bug/20720.mtail
```
