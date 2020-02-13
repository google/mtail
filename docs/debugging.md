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


