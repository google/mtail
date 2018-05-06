#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

start_server --progs $PROGS --logs $LOGS/*

uri_get /debug/vars
expect_json_field_eq '{}' prog_loads_total "${WGET_DATA}"

touch $PROGS/nocode.mtail
uri_get /debug/vars
expect_json_field_eq '{  "nocode.mtail": 1}' prog_loads_total "${WGET_DATA}"

pass
