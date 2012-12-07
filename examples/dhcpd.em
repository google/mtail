# Copyright 2011 Google Inc. All Rights Reserved.
# This file is available under the Apache license.

counter request_total by command
counter config_file_errors
counter peer_disconnects
counter dhcpdiscovers by mac
counter bind_xid_mismatch
counter duplicate_lease
counter bad_udp_checksum
counter unknown_subnet
counter dhcpdiscover_nofree by network
counter unknown_lease by ip
counter update_rejected
counter failover_peer_timeout
counter ip_already_in_use
counter ip_abandoned by reason
counter invalid_state_transition
counter negative_poolreq by pool
counter lease_conflicts

# Syslog decorator
def syslog {/(?P<date>(?P<legacy_date>\w+\s+\d+\s+\d+:\d+:\d+)|(?P<rfc3339_date>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d+[+-]\d{2}:\d{2}))/ +
    /\s+(?:\w+@)?(?P<hostname>[\w\.-]+)\s+(?P<application>[\w\.-]+)(?:\[(?P<pid>\d+)\])?:\s+(?P<message>.*)/ {
        len($legacy_date) > 0 {
            strptime($2, "Jan _2 15:04:05")
        }
        len($rfc3339_date) > 0 {
            strptime($rfc3339_date, "2006-01-02T03:04:05-0700")
        }
        next
    }
}

const IP /\d+(\.\d+){3}/
const MATCH_IP /(?P<ip>/ + IP + /)/
const MATCH_NETWORK /(?P<network>\d+(\.\d+){1,3}\/\d+)/
const MATCH_MAC /(?P<mac>([\da-f]{2}:){5}[\da-f]{2})/
    
@syslog {
    # Request
    /(balanced|balancing|bootreply|bootrequest|dhcpack|dhcpdecline|dhcpdiscover|dhcpinform|dhcpnak|dhcpoffer|dhcprelease|dhcprequest)/ {
        request_total[tolower($1)]++

        # DHCP Discover
        /DHCPDISCOVER from / + MATCH_MAC {
            dhcpdiscovers[$mac]++

            /network / + MATCH_NETWORK + /: no free leases/ {
                dhcpdiscover_nofree[$network]++
            }
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

    # XID mismatches
    /bind update on / + IP + / got ack from (?P<group>\w+): xid mismatch./ {
        bind_xid_mismatch++
    }

    # Duplicate lease
    /uid lease / + MATCH_IP + / for client / + MATCH_MAC + / is duplicate on / + MATCH_NETWORK {
        duplicate_lease++
    }

    # Bad UDP Checksum
    /(?P<count>\d+) bad udp checksums in \d+ packets/ {
        bad_udp_checksum += $count
    }

    # Unknown subnet
    /DHCPDISCOVER from / + MATCH_MAC + / via / + IP + /: unknown network segment/ {
        unknown_subnet++
    }

    # Unknown lease
    /DHCPREQUEST for / + IP + /\(/ + IP + /\) from / + MATCH_MAC + / via / + IP + /: unknown lease / + MATCH_IP {
        unknown_lease[$ip]++
    }

    # Update rejected
    /bind update on \S+ from \S+ rejected: incoming update is less critical than the outgoing update/ {
        update_rejected++
    }
    
    /timeout waiting for failover peer \S+/ {
        failover_peer_timeout++
    }
    
    /ICMP Echo reply while lease / + IP + /valid/ {
        ip_already_in_use++
    }
    
    /unexpected ICMP Echo reply from / + IP {
        ip_already_in_use++
    }
    
    /Abandoning IP address / + IP + /: (?P<reason>.*)/ {
        ip_abandoned[$reason]++
    }

    /bind update on \S+ from \S+ rejected: / + IP + /: invalid state transition/ {
        invalid_state_transition++
    }

    /peer (?P<pool>[^:]+): Got POOLREQ, answering negatively!/ {
        negative_poolreq[$pool]++
    }

    /Lease conflict at/ {
        lease_conflicts++
    }
}
