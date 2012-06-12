# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

exported counter bytes_total by operation
internal gauge connection_time by pid
exported counter connections_total
exported counter connection_time_total as "connection-time_total"
exported counter transfers_total by operation, module

/^(?P<date>\d+\/\d+\/\d+ \d+:\d+:\d+) \[(?P<pid>\d+)\] / {
  strptime($date, "2006/01/02 15:04:05")

  # Transfer log
  # %o %h [%a] %m (%u) %f %l
  /(?P<operation>\S+) (\S+) \[\S+\] (?P<module>\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
    transfers_total[$operation][$module] += 1
  }

  # Connection
  /connect from \S+ \(\d+\.\d+\.\d+\.\d+\)/ {
    connections_total ++
    connection_time[$pid] = timestamp
  }

  # Connection summary
  /sent (?P<sent>\d+) bytes  received (?P<received>\d+) bytes  total size \d+/ {
    bytes_total["sent"] += $sent

    bytes_total["received"] += $received

    connection_time_total += timestamp - connection_time[$pid]
    # unset connection_time[$pid]
  }
}
