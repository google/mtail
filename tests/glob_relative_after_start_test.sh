#!/bin/bash

# Test that files created after startup are correctly matched against the glob pattern, if the pattern is relative.

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

cd $LOGS

touch $LOGS/log.1.txt

start_server  --logs "log.*" --progs $PROGS

uri_get /debug/vars
expect_json_field_eq "1" log_count "${DATA}"
expect_json_field_eq "0" line_count "${DATA}"

echo "line 1" >> $LOGS/log.1.txt
sleep 1

uri_get /debug/vars
expect_json_field_eq "1" log_count "${DATA}"
expect_json_field_eq "1" line_count "${DATA}"

echo "line 1" >> $LOGS/log.2.txt
sleep 1

uri_get /debug/vars
expect_json_field_eq "2" log_count "${DATA}"
expect_json_field_eq "2" line_count "${DATA}"

echo "line 2" >> $LOGS/log.2.txt
sleep 1

uri_get /debug/vars
expect_json_field_eq "2" log_count "${DATA}"
expect_json_field_eq "3" line_count "${DATA}"

pass
