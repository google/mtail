#!/bin/bash

for test in $(dirname $0)/*_test.sh ; do
    echo -n "${test}... "
    ${test}
    if [[ $? -eq 0 ]]; then
        echo "PASS"
    fi
done
