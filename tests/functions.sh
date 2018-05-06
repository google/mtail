#!/bin/bash

if (( MTAIL_TEST_FUNCTIONS_SH__++ == 0 )); then

ATEXIT="${ATEXIT-}"
atexit () {
    if [ -z "$ATEXIT" ]; then
        ATEXIT="$1"
    else
        ATEXIT="$1 ; $ATEXIT"
    fi
    trap "$ATEXIT" EXIT
}

if [ -z "${TEST_TMPDIR:-}" ]; then
    export TEST_TMPDIR="$(mktemp -d ${TMPDIR:-/tmp}/mtail-test.XXXXXXXX)"
fi
if [ ! -e "${TEST_TMPDIR}" ]; then
  mkdir -p -m 0700 "${TEST_TMPDIR}"
  # Clean TEST_TMPDIR on exit
  atexit "rm -fr ${TEST_TMPDIR}"
fi

LOGS=${TEST_TMPDIR}/logs
mkdir -p $LOGS
MTAIL_ARGS="\
    --progs examples/linecount.mtail \
    --logs $LOGS/* \
    --log_dir ${TEST_TMPDIR} \
    -v 1 \
"

append() {
    local line="$*"
    echo "$line" >> $LOGS/log
}

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

fail() {
    local msg="$*"
    echo "FAILED: $msg"
    exit 1
}

start_server() {
    extra_args=$*
    MTAIL_PORT=$(pick_random_unused_tcp_port)
    MTAIL_ARGS="--port ${MTAIL_PORT} $MTAIL_ARGS"
    mtail $MTAIL_ARGS $extra_args &
    MTAIL_PID=$!
    # wait for http port to respond, or sleep 1
    sleep 1
    atexit 'kill ${MTAIL_PID:?}'
}

WGET_ARGS="\
--no-netrc \
--quiet \
--tries=1 \
--output-document=- \
"

uri_get() {
    local path=$1
    local uri="http://localhost:${MTAIL_PORT}${path}"
    WGET_DATA=$(wget ${WGET_ARGS} ${uri})
    result=$?
    if [ $result -ne 0 ]; then
        fail "wget failed with error $result"
    fi
}

expect_str_in () {
    local needle="$1"
    local haystack="$2"
    echo "${haystack:?}" | grep -F -q "${needle:?}"
    if [ $? -ne 0 ]; then
        fail "\"$needle\" not found in \"$haystack\""
    fi
}

pass() {
    echo "PASSED"
    exit 0
}

fi  # include guard
