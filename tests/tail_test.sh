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

#
# Find a random unused TCP port
#
pick_random_unused_tcp_port () {
    perl -MSocket -e '
sub CheckPort {
  my ($port) = @_;
  socket(TCP_SOCK, PF_INET, SOCK_STREAM, getprotobyname("tcp"))
    || die "socket(TCP): $!";
  setsockopt(TCP_SOCK, SOL_SOCKET, SO_REUSEADDR, 1)
    || die "setsockopt(TCP): $!";
  return 0 unless bind(TCP_SOCK, sockaddr_in($port, INADDR_ANY));
  socket(UDP_SOCK, PF_INET, SOCK_DGRAM, getprotobyname("udp"))
    || die "socket(UDP): $!";
  return 0 unless bind(UDP_SOCK, sockaddr_in($port, INADDR_ANY));
  return 1;
}
for (1 .. 128) {
  my ($port) = int(rand() * 27000 + 32760);
  if (CheckPort($port)) {
    print "$port\n";
    exit 0;
  }
}
print "NO_FREE_PORT_FOUND\n";
exit 1;
'
}

start_server() {
    extra_args=$*
    MTAIL_PORT=$(pick_random_unused_tcp_port)
    MTAIL_ARGS="--port ${MTAIL_PORT} $MTAIL_ARGS"
    mtail $MTAIL_ARGS $extra_args &
    MTAIL_PID=$!
    # wait for http port to respond, or sleep 1
    sleep 1
}

WGET_ARGS="\
--no-netrc \
--quiet \
--tries=1 \
--output-document=- \
"

uri_get() {
    uri="http://localhost:${MTAIL_PORT}$1"
    WGET_DATA=$(wget ${WGET_ARGS} ${uri})
    result=$?
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
uri_get /debug/vars
echo ${WGET_DATA} | grep line_count
echo ${WGET_DATA} | grep log_count
# expecting 3 line count, 1 log count

echo "metrics:"
uri_get /metrics
echo ${WGET_DATA} | grep line_count

# 3xpecting line_count = 3


teardown
