#!/bin/bash

if (( MTAIL_TEST_FUNCTIONS_SH__++ == 0 )); then

# Put atexit functions on a stack and call them all; like trap but doesn't overwrite previous traps.
ATEXIT="${ATEXIT-}"
atexit () {
    if [ -z "$ATEXIT" ]; then
        ATEXIT="$1"
    else
        ATEXIT="$1 ; $ATEXIT"
    fi
    trap "$ATEXIT" EXIT
}

# Set up a temporary test directory
if [ -z "${TEST_TMPDIR:-}" ]; then
    export TEST_TMPDIR="$(mktemp -d ${TMPDIR:-/tmp}/mtail-test.XXXXXXXX)"
fi
if [ ! -e "${TEST_TMPDIR}" ]; then
  mkdir -p -m 0700 "${TEST_TMPDIR}"
  # Clean TEST_TMPDIR on exit
  atexit "rm -fr ${TEST_TMPDIR}"
fi

# Default mtail parameters for start_server
MTAIL_ARGS="\
    --log_dir ${TEST_TMPDIR} \
    --alsologtostderr \
    -v=2
"

# Find a random unused TCP port
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
    echo "stderr follows:"
    cat ${TEST_TMPDIR}/stderr
    exit 1
}

pass() {
    echo "PASSED"
    exit 0
}

skip() {
    echo "SKIPPED: $*"
    exit 0
}

# Start an mtail server with default args and extra args on a random port.
start_server() {
    bg=1
    if [ $1 = "nobg" ]; then
        shift
        bg=0
    fi
    MTAIL_PORT=$(pick_random_unused_tcp_port)
    MTAIL_ARGS="--port ${MTAIL_PORT} $MTAIL_ARGS"
    if [ $bg -eq 1 ]; then
        ${MTAIL_BIN:-mtail} $MTAIL_ARGS "$@" 2>${TEST_TMPDIR}/stderr &
        MTAIL_PID=$!
        # wait for http port to respond, or sleep 1
        sleep 1
        atexit 'kill ${MTAIL_PID:?}'
    else
        ${MTAIL_BIN:-mtail} $MTAIL_ARGS "$@" 2>${TEST_TMPDIR}/stderr
    fi
}

# Default parameters for curl
CURL_ARGS="\
--silent \
"

# Get a page from mtail's http server, storing it in $DATA
uri_get() {
    local path=$1
    local uri="http://localhost:${MTAIL_PORT}${path}"
    DATA=$(curl ${CURL_ARGS} ${uri})
    result=$?
    if [ $result -ne 0 ]; then
        fail "curl failed with error $result"
    fi
}

# Expect to find a string needle in a longer string haystack
expect_str_in () {
    local needle="$1"
    local haystack="$2"
    echo "${haystack:?}" | grep -F -q "${needle:?}"
    if [ $? -ne 0 ]; then
        fail "\"$needle\" not found in \"$haystack\""
    fi
}

# Using jq, get a field from json.
get_json_field() {
    local field_name="$1"
    local json="$2"
    echo "${json}" | jq ".${field_name}"
}

# If a program doesn't exist on PATH, skip this test
skip_without() {
    hash "$1" 2>/dev/null
    if [[ $? -ne 0 ]]; then
        skip "$1 not found"
    fi
}

# Expect two parameters are equal
expect_eq() {
    local expected="$1"
    local received="$2"
    if [[ "$expected" != "$received" ]]; then
        fail "$3: expected $expected received $received"
    fi
}

# Expect that a json field in a message is some value
expect_json_field_eq() {
    local expected="$1"
    local field_name="$2"
    local json="$3"
    expect_eq "$expected" "$(get_json_field ${field_name} "${json}" | tr -d '\n')" ${field_name}
}

fi  # include guard
