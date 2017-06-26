# Introduction

As `mtail` is designed to tail log files and apply regular expressions to new log lines to extract data, the language naturally follows this pattern-action style.

It resembles another, more famous pattern-action language, that of AWK.

This page errs on the side of a language specification and reference.  See the [[Programming Guide]] for a gentler introduction to writing `mtail` programs.

# Details

`mtail` runs all programs on every line received by the log tailing subsystem.  The rough model of this looks like:

```
for line in lines:
  for regex in regexes:
    if match:
      do something
```

Thus, it is useful to keep in mind that each program is acting on a single line of log data, then terminates.

## Exported Variables

Variables, which have type `counter` or `gauge`, must be declared before their use.

```
counter line_count
gauge queue_length
```

They can be exported with a different name, with the `as` keyword, if one wants to use characters that would cause a parse error.

```
counter line_count as "line-count"
```

Variables can be dimensioned with one or more axes, with the `by` keyword.

```
counter bytes by operation, direction
```

Putting the `hidden` keyword at the start of the declaration means it won't be exported, which can be useful for storing temporary information.

```
hidden counter login_failures
```

## Pattern/Actions

mtail programs look a lot like awk programs.  They consist of a list of conditional expressions followed by a brace-delimited block of code:
```
COND {
  ACTION
}
```

COND is a conditional expression.  It can be a regular expression, which if matched, enters the action block, or a relational expression, as you might see in a C program's `if` statement:

```
/foo/ {
  ACTION1
}

variable > 0 {
  ACTION2
}
```

In the above program, ACTION1 is taken on each line input if that line matches the word `foo`, and ACTION2 is taken on each line if when that line is read, the variable `variable` is greater than 0.

The action statements must be wrapped in curly braces, i.e. `{}`.  `mtail` programs have no single-line statement conditionals like C.

## Single definition of constants

To re-use regular expressions, you can assign them to a `const` identifier:

```
const PREFIX /^\w+\W+\d+ /

// + PREFIX {
  ACTION1
}

// + PREFIX + /foo/ {
  ACTION2
}
```

In this example, ACTION1 is done for every line that starts with the prefix regex, and ACTION2 is done for the subset of those lines that also contain 'foo'.

## Else Clauses

When a conditional expression does not match, action can be taken as well:

```
/foo/ {
  ACTION1
} else {
  ACTION2
}
```

Else clauses can be nested. There is no ambiguity with the dangling-else problem, as `mtail` programs must wrap all block statements in `{}`.

## Incrementing a Counter

The simplest `mtail` program merely counts lines read:
```
/$/ {
  line_count++
}
```

This program instructs `mtail` to increment the `line_count` counter variable on every line received (specifically anytime an end-of-line is matched.)

## Advanced conditionals

The `otherwise` keyword can be used as a conditional statement. It matches if no preceding conditional in the current scope has matched. This functions like the "default" clause in a switch statement in a C-like language.

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

In this example, ACTION3 will be executed if neither `/foo1/` or `/foo2/` match on the input, but `/foo/` does.

## Capture Groups

Regular expressions can contain capture groups, subexpressions wrapped in parentheses.  These can be referred to in the action block to extract data from the line being matched.

For example, part of a program that can extract from rsyncd logs may want to break down transfers by operation and module.
```
counter transfers_total by operation, module

/(?P<operation>\S+) (\S+) \[\S+\] (\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
    transfers_total[$operation][$3]++
}
```

Numeric capture groups address subexpressions in the match result as you might expect from grouping in awk and perl.

Named capture groups can be referred to by their name as indicated in the regular expression using the `?P<name>` notation, as popularised by the Python regular expression library.

## Numerical capture groups and Metric type information

By limiting the pattern of a capturing group to only numeric characters, the programmer can hint to mtail about the type of an expression.  For example, in the regular expression

`/(\d+)/`

the first capture group can only match digits, and so the compiler will infer that this is an integer match.

`/(\d+\.\d+)/`

looks like it matches floating point numbers, and so the compiler will infer that this is of type float.

The compiler performs type inference on the expressions that use the capture groups, and the metrics they are ultimately assigned to, and will assign a type (either integer or floating point) to the metrics exported.

Thus in a program like:

```
gauge i
gauge f

/(\d+)/ {
  i = $1
}

/(\d+\.\d+)/ {
  i = $1
}
```

the metric `i` will be of type Int and the metric `f` will be of type Float.



## Timestamps

It is also useful to timestamp a metric with the time the application thought an event occurred.  Logs typically prefix the log line with a timestamp string, which can be extracted and then parsed into a timestamp internally, with the `strptime` builtin function.

A regular expression that extracts the timestamp in boring old syslog format looks like:
```
/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
  strptime($date, "Jan 02 15:04:05")

  ...
}
```

Buyer beware!  The format string used by `mtail` is the same as the [Go time.Parse() format string](http://golang.org/src/pkg/time/format.go), which is completely unlike that used by C's strptime.  The format string must always be the 2nd of January 2006 at 3:04:05 PM.  See the documentation for the **ANSIC** format in the above link for more details.

## Nested Actions

It is of course possible to nest pattern-actions within actions.  This lets you factor out common prefixes (or suffixes!) and deal with per-message actions separately.

For example, parsing syslog timestamps is something you only want to do once.
```
counter foo
counter bar

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

This will result in both foo and bar counters being timestamped with the current log line's parsed time, once they match a line.

# Builtin functions

A few builtin functions exist for manipulating the virtual machine state.  They
are:

1. `timestamp()`, a function of no arguments, which returns the current
   timestamp.  This is undefined if neither `settime` or `strptime` have been
   called previously.
1.  `len(x)`, a function of one string argument, which returns the length of
    the string argument.
1. `settime(x)`, a function of one integer argument, which sets the current
   timestamp
1.  `strptime(x, y)`, a function of two string arguments, which parses the
    timestamp in the string `x` with the parse format string in `y`, and sets
    the current timestamp.  The parse format string must follow [Go's
    time.Parse() format string](http://golang.org/src/pkg/time/format.go)
1. `tolower(x)`, a function of one string argument, which lowercases the
   string.

As described in Nested Actions, the current timestamp refers to `mtail`'s idea of
the time associated with the current log line.  This timestamp is used when the
variables are exported to the upstream collector.  This defaults to the time
that the log line arrives in `mtail`, and can be changed with the `settime()` or
`strptime()` builtins.

User defined functions are not supported, but read on to Decorated Actions for
how to factor out common matching pattern/action behaviour.

# Decorated actions

Decorated actions are an inversion of nested actions.  They allow the program
to define repetetive functions that perform the same extraction across many
different actions.

For example, most log file formats start with a timestamp prefix.  To reduce
dupliation of work, decorators can be used to factor out the common work of
extracting the timestamp.  For example, to define a decorator, use the `def`
keyword:

```
def syslog {
  /(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
    strptime($date, "Jan  2 15:04:05")
    next
  }
}
```

The decorator definition starts and ends in a curly-braced block, and looks
like a normal pattern/action as above.  The new part is the `next` keyword,
which indicates to `mtail` where to jump into the *decorated* block.

To use a decorator:

```
@syslog {
    /some event/ {
      variable++
    }
}
```

The `@` notation, familiar to Python programmers, denotes that this block is
"wrapped" by the `syslog` decorator.  The syslog decorator will be called on
each line first, which extracts the timestamp of the log line.  Then, `next`
causes the wrapped block to execute, so then `mtail` matches the line against the
pattern `some event`, and if it does match, increments `variable`.

# Metric Storage Management

mtail performs no implicit garbage collection in the metric storage.  The
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

In this example, a hidden metric is used to record some internal state.  It
will grow unbounded as the number of sessions increases.  If the programmer
knows that the `/end/` pattern is the last time a session will be observed,
then the datum at `$session` will be freed, which keeps `mtail` memory usage
under control and will improve search time for finding dimensioned metrics.
