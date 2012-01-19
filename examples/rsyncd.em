# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

# %d [%p]
/^(?P<date>\d+\/\d+\/\d+ \d+:\d+:\d+) \[(?P<pid>\d+)\] / {
  strptime($1, "2006/01/02 15:04:05")

  # Transfer log
  # %o %h [%a] %m (%u) %f %l
  /(?P<operation>\S+) (\S+) \[\S+\] (?P<module>\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
    tag(operation, $1)
    tag(module, $3)
    inc(transfers_total)
  }

  # Connection
  /connect from \S+ \(\d+\.\d+\.\d+\.\d+\)/ {
    inc(connections_total)
    #push timestamp connections[$pid]
  }

  # Connection summary
  /sent (?P<sent>\d+) bytes  received (?P<received>\d+) bytes  total size \d+/ {
    tag(operation, sent)
    inc(bytes_total, $1)

    tag(operation, received)
    inc(bytes_total, $2)

    # FIXME
    # TODO(jaq): blurgh
    #inc(connection-time_total, timestamp - connections[$pid])
    #pop connections[$pid]
  }
}
