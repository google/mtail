# mtail Language

## Description

As `mtail` is designed to tail log files and apply regular expressions to new
log lines to extract data, the language naturally follows this pattern-action
style.

It resembles another, more famous pattern-action language, that of AWK.

This page errs on the side of a language specification and reference. See the
[Programming Guide](Programming-Guide.md) for a gentler introduction to writing
`mtail` programs.

## Program Execution

`mtail` runs all programs on every line received by the log tailing subsystem.
The rough model of this looks like:

```
for line in lines:
  for regex in regexes:
    if match:
      do something
```

Each program operates once on a single line of log data, and then terminates.

## Program Structure

An `mtail` program consists of exported variable definitions, pattern-action
statements, and optional decorator definitions.

```
exported variable

pattern {
  action statements
}

def decorator {
  pattern and action statements
}
```

## Exported Variables

`mtail`'s purpose is to extract information from logs and deliver them to a
monitoring system. Thus, variables must be named for export.

Variables, which have type `counter` or `gauge`, must be declared before their
use.

```
counter line_count
gauge queue_length
```

They can be exported with a different name, with the `as` keyword, if one wants
to use characters that would cause a parse error. This example causes the metric
to be named `line-count` in the collecting monitoring system.

```
counter line_count as "line-count"
```

Variables can be dimensioned with one or more axes, with the `by` keyword,
creating multidimensional data. Dimensions can be used for creating histograms,
as well.

```
counter bytes by operation, direction
counter latency_ms by bucket
```

Putting the `hidden` keyword at the start of the declaration means it won't be
exported, which can be useful for storing temporary information. This is the
only way to share state between each line being processed.

```
hidden counter login_failures
```

## Pattern/Action form.

`mtail` programs look a lot like `awk` programs. They consist of a conditional
expression followed by a brace-enclosed block of code:

```
COND {
  ACTION
}
```

`COND` is a conditional expression. It can be a regular expression, which if
matched enters the action block, or a relational expression as you might
encounter in a C program's `if` statement (but without the `if`, it is
implicit.)

```
/foo/ {
  ACTION1
}

variable > 0 {
  ACTION2
}

/foo/ && variable > 0 {
  ACTION3
}
```

In the above program, ACTION1 is taken on each line input if that line matches
the word `foo`, and ACTION2 is taken on each line if when that line is read, the
variable `variable` is greater than 0. ACTION3 occurs if both are true.

The action statements must be wrapped in curly braces, i.e. `{}`. `mtail`
programs have no single-line statement conditionals like C.

### Regular Expressions

`mtail` supports RE2-style regular expression syntax, but is limited by what is
supported by the Go implementation of [Go's
regexp/syntax](https://godoc.org/regexp).

#### Constant pattern fragments

To re-use parts of regular expressions, you can assign them to a `const` identifier:

```
const PREFIX /^\w+\W+\d+ /

PREFIX {
  ACTION1
}

PREFIX + /foo/ {
  ACTION2
}
```

In this example, ACTION1 is done for every line that starts with the prefix
regex, and ACTION2 is done for the subset of those lines that also contain
'foo'.

Pattern fragments like this don't need to be prefixes, they can be anywhere in the expression.

```
counter maybe_ipv4

const IPv4 /(?P<ip>\d+\.\d+\.\d+\.\d+)/

/something with an / + IPv4 + / address/ {
  maybe_ipv4++
}
```

See [dhcpd.mtail](../examples/dhcpd.mtail) for more examples of this.

See also the section on decorators below for improving readability of
expressions that are only matched once.

### Conditionals

More complex expressions can be built up from relational expressions and other
pattern expressions.

#### Operators

The following relational operators are available in `mtail`:

*   `<` less than
*   `<=` less than or equal
*   `>` greater than
*   `>=` greater than or equal
*   `==` is equal
*   `!=` is not equal
*   `=~` pattern match
*   `!~` negated pattern match
*   `||` logical or
*   `&&` logical and
*   `!` unary logical negation

The following arithmetic operators are available in `mtail`:

*   `|` bitwise or
*   `&` bitwise and
*   `^` bitwise xor
*   `+` addition
*   `-` subtraction
*   `*` multiplcation
*   `/` division
*   `<<` bitwise shift left
*   `>>` bitwise shift right
*   `**` exponent

The following arithmetic operators act on exported variables.

*   `=` assignment
*   `++` increment
*   `+=` increment by
*   `--` decrement

#### `else` Clauses

When a conditional expression does not match, action can be taken as well:

```
/foo/ {
  ACTION1
} else {
  ACTION2
}
```

Else clauses can be nested. There is no ambiguity with the dangling-else
problem, as `mtail` programs must wrap all block statements in `{}`.

#### `otherwise` clauses

The `otherwise` keyword can be used as a conditional statement. It matches if no
preceding conditional in the current scope has matched. This behaves similarly
to the `default` clause in a C `switch` statement.

```
/foo/ {
  /foo1/ {
     ACTION1
  }
  /foo2/ {
     ACTION2
  }
  otherwise {
     ACTION3
  }
}
```

In this example, ACTION3 will be executed if neither `/foo1/` or `/foo2/` match
on the input, but `/foo/` does.

### Actions

#### Incrementing a Counter

The simplest `mtail` program merely counts lines read:

```
/$/ {
  line_count++
}`
```

This program instructs `mtail` to increment the `line_count` counter variable on
every line received (specifically anytime an end-of-line is matched.)

#### Capture Groups

Regular expressions in patterns can contain capture groups -- subexpressions
wrapped in parentheses. These can be referred to in the action block to extract
data from the line being matched.

For example, part of a program that can extract from `rsyncd` logs may want to
break down transfers by operation and module.

```
counter transfers_total by operation, module

/(?P<operation>\S+) (\S+) \[\S+\] (\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
  transfers_total[$operation][$3]++
}
```

Or, the value of the counter can be increased by the value of a capture group:

```
counter bytes_total by operation, module

/(?P<operation>\S+) (\S+) \[\S+\] (\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
  bytes_total[$operation][$3] += $bytes
}
```

Numeric capture groups address subexpressions in the match result as you might
expect from regular expression groups in other languages, like awk and perl --
e.g. the expression `$3` refers to the third capture group in the regular
expression.

Named capture groups can be referred to by their name as indicated in the
regular expression using the `?P<name>` notation, as popularised by the Python
regular expression library -- e.g. `$bytes` refers to `(?P<bytes>\d+)` in the
examples above.

Capture groups can be used in the same expression that defines them, for example
in this expression that matches and produces `$x`, then compares against that
value.

```
/(?P<x>\d+)/ && $x > 1 {
  nonzero_positives++
}
```

#### Timestamps

It is also useful to timestamp a metric with the time the application thought an
event occurred. Logs typically prefix the log line with a timestamp string,
which can be extracted and then parsed into a timestamp internally, with the
`strptime` builtin function.

A regular expression that extracts the timestamp in boring old syslog format
looks like:

```
/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
  strptime($date, "Jan 02 15:04:05")
  ...
}
```

Buyer beware! The format string used by `mtail` is the same as the [Go
time.Parse() format string](https://godoc.org/time#Parse), which is completely
unlike that used by C's strptime. The format string must always be the 2nd of
January 2006 at 3:04:05 PM. See the documentation for the **ANSIC** format in
the above link for more details. **NOTE** that *unlike* Go's `time.Parse()` (and
*like* C's) the format string is the *second* argument to this builtin function.

> NOTE: without a `strptime()` call, `mtail` will default to using the current
> system time for the timestamp of the event. This may be satisfactory for
> near-real-time logging.

#### Nested Actions

It is of course possible to nest more pattern-actions within actions. This lets
you factor out common parts of a match expression and deal with per-message
actions separately.

For example, parsing syslog timestamps is something you may only wish to do
once, as it's expensive to match (and difficult to read!)

```
counter foo counter bar

/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
  strptime($date, "Jan 02 15:04:05")

  /foo/ {
    foo++
  }

  /bar/ {
    bar++
  }
}
```

This will result in both foo and bar counters being timestamped with the current
log line's parsed time, once they match a line.

#### Decorated actions

Decorated actions are an inversion of nested actions. They allow the program to
define repetetive functions that perform the same extraction across many
different actions.

For example, most log file formats start with a timestamp prefix. To reduce
dupliation of work, decorators can be used to factor out the common work of
extracting the timestamp. For example, to define a decorator, use the `def`
keyword:

```
def syslog {
  /(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
    strptime($date, "Jan  2 15:04:05")
    next
  }
}
```

The decorator definition starts and ends in a curly-braced block, and looks like
a normal pattern/action as above. The new part is the `next` keyword, which
indicates to `mtail` where to jump into the *decorated* block.

To use a decorator:

```
@syslog {
  /some event/ {
    variable++
  }
}
```

The `@` notation, familiar to Python programmers, denotes that this block is
"wrapped" by the `syslog` decorator. The syslog decorator will be called on each
line first, which extracts the timestamp of the log line. Then, `next` causes
the wrapped block to execute, so then `mtail` matches the line against the
pattern `some event`, and if it does match, increments `variable`.

#### Types

`mtail` has a few internal types on two dimensions.

The first dimension has no bearing on the behaviour of `mtail`, but changes how
the variables are exported:

*   `counter` assumes that the variable is a monotonically increasing measure,
    so that computations on sampled data like rates can be performed without
    loss. Use for counting events or summing up bytes transferred.
*   `gauge` assumes that the variable can be set to any value at any time,
    signalling that rate computations are risky. Use for measures like queue
    length at a point in time.

The second dimension is the internal representation of a value, which is used by
`mtail` to attempt to generate efficient bytecode.

*   Integer
*   Float
*   Bool
*   String

Some of these types can only be used in certain locations -- for example, you
can't increment a counter by a string, but `mtail` will fall back to a attempt
to do so, logging an error if a runtime type conversion fails.

These types are usually inferred from use, but can be influenced by the
programmer with builtin functions. Read on.

#### Builtin functions

`mtail` contains some builtin functions for help with extracting information and
manipulating state.

There are "pure" builtin functions, in that they have no side effects on the
program state.

*   `len(x)`, a function of one string argument, which returns the length of the
    string argument `x`.
*   `tolower(x)`, a function of one string argument, which returns the input `x`
    in all lowercase.

There are type coercion functions, useful for overriding the type inference made
by the compiler if it chooses badly. (If the choice is egregious, please file a
bug!)

*   `int(x)`, a function of one argument performs type conversion to integer. If
    `x` is a type that can be converted to integer, it does so. If the type of
    `x` cannot be converted to an integer, a compile error is triggered. If the
    valye of `x` cannot be converted to an integer, then a runtime error is
    triggered.
*   `float(x)`, a function of one argument that performs type conversion to
    floating point numbers. The same rules apply as for `int()` above.
*   `string(x)`, a function of one argument that performs conversion to string
    values.
*   `strtol(x, y)`, a function of two arguments, which converts a string `x` to
    an integer using base `y`. Useful for translating octal or hexadecimal
    values in log messages.

A few builtin functions exist for manipulating the virtual machine state as side
effects for the metric export.

*   `getfilename()`, a function of no arguments, which returns the filename from
    which the current log line input came.
*   `settime(x)`, a function of one integer argument, which sets the current
    timestamp register.
*   `strptime(x, y)`, a function of two string arguments, which parses the
    timestamp in the string `x` with the parse format string in `y`, and sets
    the current timestamp register. The parse format string must follow [Go's
    time.Parse() format string](http://golang.org/src/pkg/time/format.go)
*   `timestamp()`, a function of no arguments, which returns the current
    timestamp. This is undefined if neither `settime` or `strptime` have been
    called previously.

The **current timestamp register** refers to `mtail`'s idea of the time
associated with the current log line. This timestamp is used when the variables
are exported to the upstream collector. The value defaults to the time that the
log line arrives in `mtail`, and can be changed with the `settime()` or
`strptime()` builtins.

User defined functions are not supported, but read on to Decorated Actions for
how to reuse common code.

#### Numerical capture groups and Metric type information

By limiting the pattern of a capturing group to only numeric characters, the
programmer can provide hints to `mtail` about the type of an expression. For
example, in the regular expression

`/(\d+)/`

the first capture group can only match digits, and so the compiler will infer
that this is an integer match.

`/(\d+\.\d+)/`

looks like it matches floating point numbers, and so the compiler will infer
that this is of type float.

> NOTE: In the expression above, the dot is escaped. A regular expression
> operator `.` matches every character and so the inference assumes that the
> type of '.' is a string.

The compiler performs type inference on the expressions that use the capture
groups, and the metrics they are ultimately assigned to, and will assign a type
(either integer or floating point) to the metrics exported.

Thus in a program like:

```
gauge i
gauge f

/(\d+)/ {
  i = $1
}

/(\d+\.\d+)/ {
  f = $1
}
```

the metric `i` will be of type Int and the metric `f` will be of type Float.

The advantage of limiting pattern matches to specific values is that `mtail` can
generate faster bytecode if it knows at compile-time the types to expect. If
`mtail` can't infer the value types, they default to `String` and `mtail` will
attempt a value conversion at runtime if necessary. Runtime conversion errors
will be emitted to the standard INFO log, and terminate program exection for
that log line.

#### Variable Storage Management

`mtail` performs no implicit garbage collection in the metric storage. The
program can hint to the virtual machine that a specific datum in a dimensioned
metric is no longer going to be used with the `del` keyword.

```
gauge duration by session
hidden session_start by session

/end/ {
  duration[$session] = timestamp() - session_start[$session]

  del session_start[$session]
}
```

In this example, a hidden metric is used to record some internal state. It will
grow unbounded as the number of sessions increases. If the programmer knows that
the `/end/` pattern is the last time a session will be observed, then the datum
at `$session` will be freed, which keeps `mtail` memory usage under control and
will improve search time for finding dimensioned metrics.

`del` can be modified with the `after` keyword, signalling that the metric
should be deleted after some period of no activity.  For example, the
expression

```
  del session_start[$session] after 24h
```

would mean that the datum indexed by `$session` will be removed 24 hours after the last update is recorded.

The del-after form takes any time period supported by the go
[`time.ParseDuration`](https://golang.org/pkg/time/#ParseDuration) function.
