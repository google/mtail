#!/bin/bash

TEST_TMPDIR=/tmp/test
LOGS=$TEST_TMPDIR/logs
MTAIL_ARGS="\
    --progs examples/linecount.mtail \
    --logs $LOGS/* \
    --logtostderr \
    -v 1 \
"

append() {
    echo "APPENDING"
    echo "$*" >> $LOGS/log
}


setup() {
mkdir -p $LOGS
}

teardown() {
    kill ${MTAIL_PID?}
    rm -rf ${TEST_TMPDIR?}
}

trap teardown EXIT INT KILL

start_server() {
    extra_args=$*
    mtail $MTAIL_ARGS $extra_args &
    MTAIL_PID=$!
    # wait for http port to respond, or sleep 1
    sleep 1
}

setup
start_server --vmodule=tail=2,log_watcher=2

append 1
sleep 1
append 2
sleep 1
append 3
sleep 2

echo "debug/vars:"
wget --no-netrc -q -O- http://localhost:3903/debug/vars | grep count
# expecting 3 line count, 1 log count

echo "metrics:"
wget --no-netrc -q -O- http://localhost:3903/metrics
# 3xpecting line_count = 3


teardown
