#!/bin/bash

source $(dirname $0)/functions.sh

start_server --vmodule=tail=2,log_watcher=2

append 1
sleep 1
append 2
sleep 1
append 3
sleep 2

uri_get "/metrics"
expect_str_in 'line_count{prog="linecount.mtail"} 3' "${WGET_DATA}"

pass
