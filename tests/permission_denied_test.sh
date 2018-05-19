#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

touch $LOGS/log
chmod 000 $LOGS/log

atexit 'chmod u+r $LOGS/log'

start_server  --logs $LOGS/log --progs $PROGS --stderrthreshold=FATAL

uri_get /debug/vars
expect_json_field_eq "{  \"$LOGS/log\": 1}" log_errors_total "${WGET_DATA}"

pass
