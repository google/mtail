#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

touch $LOGS/log

start_server  --logs $LOGS/log --progs $PROGS

echo "line 1" >> $LOGS/log

uri_get /debug/vars
expect_json_field_eq 1 line_count "${WGET_DATA}"

echo -n "line " >> $LOGS/log

uri_get /debug/vars
expect_json_field_eq 1 line_count "${WGET_DATA}"

echo 2 >> $LOGS/log

uri_get /debug/vars
expect_json_field_eq 2 line_count "${WGET_DATA}"

pass
