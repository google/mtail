#!/bin/bash

export GOBIN="$(mktemp -d ${TMPDIR:-/tmp}/mtail-test-binary.XXXXXXXX)"
go install ./cmd/mtail
export MTAIL_BIN=${GOBIN}/mtail
trap "rm -rf $GOBIN" EXIT

ret=0

for test in $(dirname $0)/*_test.sh ; do
    echo -n "${test}... "
    ${test}
    if [[ $? -ne 0 ]]; then
        ret=1
    fi
done

exit $ret
