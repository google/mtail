#!/bin/bash -x 

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

mkfifo $LOGS/logpipe

start_server --logtostderr -vmodule=tail=2 --progs $PROGS --logs $LOGS/*

echo 1 >> $LOGS/logpipe

uri_get /debug/vars
expect_json_field_eq 1 line_count "${DATA}"

cat >> $LOGS/logpipe <<EOF
2
3
EOF

uri_get /debug/vars
expect_json_field_eq 3 line_count "${DATA}"

pass
