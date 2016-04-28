# Introduction

As mtail is designed to tail log files and apply regular expressions to new log lines to extract data, the language naturally follows this pattern-action style.

It resembles another, more famous pattern-action language, that of AWK.

# Details

mtail runs all programs on every line received by the log tailing subsystem.  The rough model of this looks like:

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

## Incrementing a Counter

The simplest mtail program merely counts lines read:
```
/$/ {
  line_count++
}
```

This program instructs mtail to increment the `line_count` counter variable on every line received (specifically anytime an end-of-line is matched.)

## Advanced conditionals

The `otherwise` keyword can be used as a conditional statement. It matches if no preceding conditional in the current scope has matched. This functions like the "default" clause in a switch statement in a C-like language.

Coming soon: an `else` keyword to allow chaining of mutually-exclusive conditionals. (https://github.com/google/mtail/issues/18)

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

## Timestamps

It is also useful to timestamp a metric with the time the application thought an event occurred.  Logs typically prefix the log line with a timestamp string, which can be extracted and then parsed into a timestamp internally, with the `strptime` builtin function.

A regular expression that extracts the timestamp in boring old syslog format looks like:
```
/^(?P<date>\w+\s+\d+\s+\d+:\d+:\d+)/ {
  strptime($date, "Jan 02 15:04:05")

  ...
}
```

Buyer beware!  The format string used by mtail is the same as the [golang format string](http://golang.org/src/pkg/time/format.go), which is completely unlike that used by C's strptime.  The format string must always be the 2nd of January 2006 at 3:04:05 PM.  See the documentation for the **ANSIC** format in the above link for more details.

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
