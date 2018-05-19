#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
mkdir -p $LOGS

start_server --vmodule=tail=2,log_watcher=2 --progs examples/linecount.mtail --logs $LOGS/*

echo 1 >> $LOGS/log
sleep 1
echo 2 >> $LOGS/log
sleep 1
echo 3 >> $LOGS/log
sleep 1

uri_get /metrics
expect_str_in 'line_count{prog="linecount.mtail"} 3' "${WGET_DATA}"

pass
