#!/bin/bash

ret=0

for test in $(dirname $0)/*_test.sh ; do
    echo -n "${test}... "
    ${test}
    if [[ $? -ne 0 ]]; then
        ret=1
    fi
done

exit $ret
