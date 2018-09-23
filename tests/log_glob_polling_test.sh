#!/bin/bash -x

# Test that files created after startup are correctly matched against the log pattern, in polling mode.

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

start_server  --logs "$LOGS/log*" --progs $PROGS --poll_interval=250ms

uri_get /debug/vars
expect_json_field_eq "0" log_count "${DATA}"
expect_json_field_eq "0" line_count "${DATA}"

echo "line 1" >> $LOGS/log
sleep 1

uri_get /debug/vars
expect_json_field_eq "1" log_count "${DATA}"
expect_json_field_eq "1" line_count "${DATA}"

echo "line 1" >> $LOGS/log1
sleep 2

uri_get /debug/vars
expect_json_field_eq "3" log_count "${DATA}"
expect_json_field_eq "2" line_count "${DATA}"

pass
