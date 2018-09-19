#!/bin/bash

source $(dirname $0)/functions.sh

LOGS=${TEST_TMPDIR}/logs
PROGS=${TEST_TMPDIR}/progs
mkdir -p $LOGS $PROGS

cat > $PROGS/bad.mtail <<EOF
asdfadsf
EOF
start_server nobg --progs $PROGS --logs $LOGS/* --compile_only
# r=$(wait ${MTAIL_PID})

# if [[ $r -eq 0 ]; then
#     fail "returned zero"
# fi
if ! grep -q "compile failed" ${TEST_TMPDIR}/mtail.INFO ; then
    fail "compile failed not reported in log"
fi

pass
