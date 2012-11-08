# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

counter request_total by command
counter config_file_errors
counter peer_disconnects

# Syslog decorator
def syslog {
  /(?P<date>(?P<legacy_date>\w+\s+\d+\s+\d+:\d+:\d+)|(?P<rfc3339_date>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d+[+-]\d{2}:\d{2}))\s+'(?:\w+@)?(?P<hostname>[\w\.-]+)\s+(?P<application>[\w\.-]+)(?:\[(?P<pid>\d+)\])?:\s+(?P<message>.*)/ {
    $legacy_date != "" {
        strptime($2, "Mon Jan _2 15:04:05")
    }
    $rfc3339_date != "" {
        strptime($rfc3339_date, "2006-01-02T03:04:05-0700")
    }
    next
  }
}


@syslog {
  # Request
  /(\w+)/ {
      tolower($1) != "dhcpdiscover" {
          request_total[tolower($1)]++
      }
  }

  # Config file errors
  /Configuration file errors encountered -- exiting/ {
      config_file_errors++
  }

  # Peer disconnects
  /peer ([^:]+): disconnected/ {
      peer_disconnects++
  }

  # DHCP Discover
  /DHCPDISCOVER from (?P<mac>([\da-f]{2}:){5}[\da-f]{2})/ {
      /network (?P<network>\d+(\.\d+){1,3}\/\d+): no free leases/ {
          dhcpdiscover_nofree[$network]++
      }
      # TODO(jaq): string replace mac to remove ':' ?
      dhcpdiscovers[$mac]++
  }

  # XID mismatches
  /bind update on (?P<ip>\d+(\.\d+){3}) got ack from (?P<group>\w+): xid mismatch./ {
      bind_xid_mismatch++
  }
}
