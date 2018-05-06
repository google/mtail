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
expect_json_field_eq "{  \"$LOGS/log\": 1}" log_lines_total "${WGET_DATA}"

echo "line 2" >> $LOGS/log
sleep 1

uri_get /debug/vars
expect_json_field_eq "{  \"$LOGS/log\": 2}" log_lines_total "${WGET_DATA}"

mv $LOGS/log $LOGS/log.1
echo "line 3" >> $LOGS/log
sleep 1

uri_get /debug/vars
expect_json_field_eq "{  \"$LOGS/log\": 3}" log_lines_total "${WGET_DATA}"

pass
