#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

touch $LOGS/log

start_server  --logs $LOGS/log --progs $PROGS

echo "line 1" >> $LOGS/log
sleep 1

uri_get /debug/vars
expect_json_field_eq "1" log_count "${DATA}"

echo "line 1" >> $LOGS/log1
sleep 2

uri_get /debug/vars
expect_json_field_eq "1" log_count "${DATA}"

pass
