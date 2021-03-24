# Reading `y.output`

A Yacc parser is a state machine that responds to an input stream of tokens, and has two actions:
1. **shift**, which pushes a new state on the stack
2. **reduce**, which pops a state off the stack and sets the lookahead token

[`y.output`](../internal/vm/parser/y.output) is a semi-human, semi-machine readable description of the parser state machine.  `mtail` automatically generates this during the build process with the go:generate directive in [`driver.go`](../internal/vm/parser/driver.go)

```y.output
state 0
	$accept: .start $end 
	stmt_list: .    (2)

	.  reduce 2 (src line 96)

	stmt_list  goto 2
	start  goto 1
```

There are several parts to the state described here.

The first section are the grammar rules.  The first grammar rule says that an input is accepted if we can match a start token, and then the end of the input, and we're currently (the `.`) before the start token.  The second rule has a number `(2)` (as it is the second grammar rule in the input `parser.y`, which looks like `stmt_list: stmt_list | stmt_list stmt`).  The second rule says we can be in a state where we have parsed a valid `stmt_list`.

The output always encloses grammar rules in parentheses, and state numbers are left unadorned.

The second section has the actions, and in this case there is only one that says "match any token and reduce with rule 2".  Rule 2 refers to the one in parentheses above, so it says we can match any token, pop the state off the stack, and set the lookahead token to `stmt_list`.  For our convenience it also tells us where in the source file this reduce has come from -- if we look at line 96 we'll see the grammar for parsing a `stmt_list`.  (You might wonder why the line number is in the action that uses the rule, rather than the definition of the rule in the previous section, and then you'll be in good company.)

The last section indicates what happens when we enter this state from a reduce action, although the mechanics inside the machine are identical -- if the next lookahead token is a `stmt_list`, go to state 2, and if it's a `start`, go to state 1.

For homework, look at state 1 and state 2 and describe what they mean.

Here's another example:

```y.output
state 14
	conditional_statement:  logical_expr.compound_statement ELSE compound_statement 
	conditional_statement:  logical_expr.compound_statement 
	logical_expr:  logical_expr.logical_op opt_nl bitwise_expr 

	AND  shift 48
	OR  shift 49
	MATCH  shift 50
	NOT_MATCH  shift 51
	LCURLY  shift 47
	.  error

	compound_statement  goto 45
	logical_op  goto 46
```

State 14 parses the conditional statement.  If we get here, we've already parsed a `logical_expr`, and we're trying to figure out which way to go down the parse tree.  We could find a `compound_statement`, or a `logical_op` next.

If we see an `AND`, `OR`, `MATCH`, or `NOT_MATCH` next, we **shift** to the next state, which means pushing the next state onto the stack -- the stack represents the path down the tree to get to this token.  Knowing the parser, these tokens mean we're going to parse a `logical_op`, and the difference between each is just because the parser executes a different action for each.

```y.output
state 48
	logical_op:  AND.    (26)

	.  reduce 26 (src line 202)
```

In state 48 we have recognised an `AND`, and then reduction of rule 26 says we put a `logical_op` at the front of the token stream and pop the stack (back to state 14).

The last couple of actions for state 14 say we can expect a `LCURLY` (token name for `{`, see [`lexer.go`](../internal/vm/parser/lexer.go)) and then move to state 47.  Or anything else (`.`) and we're now in an error state.


Run a parser test with debugging flags enabled, and we can see how the parser and lexer see the input:

```
go test -run TestParseInvalidPrograms/pattern_without_block --logtostderr --mtailDebug=3 --parser_test_debug
```

`mtailDebug` at 2 dumps the parser states being traversed, and 3 includes the lexer token stream as well.

The command above emits:

```
reduce 2 in:
	state-0
lex DIV(57376)
reduce 112 in:
	state-2
reduce 113 in:
	state-60
lex REGEX(57365)
lex DIV(57376)
reduce 82 in:
	state-156
reduce 69 in:
	state-35
reduce 62 in:
	state-32
lex NL(57408)
reduce 60 in:
	state-29
reduce 54 in:
	state-26
reduce 47 in:
	state-33
reduce 43 in:
	state-31
reduce 35 in:
	state-28
reduce 30 in:
	state-25
reduce 24 in:
	state-21
state-14 saw NL
error recovery pops state 14
error recovery pops state 2
error recovery pops state 0
```

We can see we start by reducing rule 2 in state 0, and then read a `DIV` token.  The trace doesn't show the **shift** actions, but we reduce through states 2, then 60.  Note that state 60 is just prior to the parser asking for the next token, indicated by the `lex REGEX` line -- this is emitted by the lexer when it returns the next token.  So we can go look at state 60 to see why we've stopped to ask for more input.

Alternatively, some grepping around for `"DIV  shift"` (with two spaces) we can see we shift to state 60 from state 65 on a `DIV` token, which helps understand where the reduces start.  Because `DIV` appears in both a regex and a division expression context, there are several matches to the grep.

The error recovery trace is interesting here, as it is a good example of what happens during the `.  error` rule.  State 14 saw a `NL` (newline) unexpectedly, so the `.` matches.  Error recovery doesn't do anything other than pop the stack until empty, so we can see the parse tree at the point of error.

This knowledge can come in handy when improving the parser error messages, using the '%error' directive in `parser.y`.  See [debugging](debugging.md) for how to use it.
