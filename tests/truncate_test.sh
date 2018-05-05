#!/bin/sh

mkdir /tmp/test

mkdir /tmp/test/logs
mkdir /tmp/test/progs

mtail --logtostderr --vmodule=tail=2,log_watcher=2 --progs /tmp/test/progs --logs /tmp/test/logs/log &
pid=$!
# wait for http port to respond, or sleep 1
sleep 1
echo 1 >> /tmp/test/logs/log
sleep 1
echo "Starting truncate"
cat /dev/null > /tmp/test/logs/log
echo "Actual truncate done"
sleep 1
echo 2 >> /tmp/test/logs/log

sleep 2

wget -q -O- http://localhost:3903/debug/vars | grep count

# expecting 2 line_count, 1 log_count

kill $pid
