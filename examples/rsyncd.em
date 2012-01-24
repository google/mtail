# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

/^(?P<date>\d+\/\d+\/\d+ \d+:\d+:\d+) \[(?P<pid>\d+)\] / {
  strptime($date, "2006/01/02 15:04:05")

  # Transfer log
  # %o %h [%a] %m (%u) %f %l
  /(?P<operation>\S+) (\S+) \[\S+\] (?P<module>\S+) \(\S*\) \S+ (?P<bytes>\d+)/ {
    tag("operation", $operation)
    tag("module", $module)
    inc("transfers_total")
  }

  # Connection
  /connect from \S+ \(\d+\.\d+\.\d+\.\d+\)/ {
    inc("connections_total")
    connections[$pid] = timestamp
  }

  # Connection summary
  /sent (?P<sent>\d+) bytes  received (?P<received>\d+) bytes  total size \d+/ {
    tag("operation", "sent")
    inc("bytes_total", $sent)

    tag("operation", "received")
    inc("bytes_total", $received)

    inc("connection-time_total", timestamp - connections[$pid])
  }
}
