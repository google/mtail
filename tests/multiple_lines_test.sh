#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

touch $LOGS/log

start_server  --logs $LOGS/log --progs $PROGS

echo 1 >> $LOGS/log

uri_get /debug/vars
expect_json_field_eq 1 line_count "${DATA}"

cat >> $LOGS/log <<EOF
2
3
EOF

uri_get /debug/vars
expect_json_field_eq 3 line_count "${DATA}"

pass
