#!/bin/sh

mkdir /tmp/test

mkdir /tmp/test/logs
mkdir /tmp/test/progs

mtail --logtostderr --vmodule=tail=2,log_watcher=2 --progs /tmp/test/progs --logs /tmp/test/logs/log &
pid=$!

echo 1 >> /tmp/test/logs/log
sleep 1
cat /dev/null > /tmp/test/logs/log
sleep 1
echo 2 >> /tmp/test/logs/log

sleep 1

wget -q -O- http://localhost:3903/debug/vars | grep count

kill $pid
