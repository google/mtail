#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/logs
mkdir -p $LOGS $PROGS

start_server --vmodule=tail=2,log_watcher=2 --progs $PROGS --logs $LOGS/log

echo 1 >> $LOGS/log
sleep 1
cat /dev/null > $LOGS/log
sleep 1
echo 2 >> $LOGS/log

sleep 1

uri_get /debug/vars
expect_eq 2 $(get_json_field line_count "${WGET_DATA}") "line_count"
expect_eq 1 $(get_json_field log_count "${WGET_DATA}") "log_count"

pass
