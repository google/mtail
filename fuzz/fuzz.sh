#!/bin/sh

set -x

d=$(dirname $0)
EMTAIL=$d/../emtail
EMGEN=$d/../emgen/emgen
OUT=$d/../fuzzout

for i in $(seq 0 99); do
    rm -rf $OUT/*
    $EMGEN --rand_seed $i > $OUT/fuzz$i.em
    $EMTAIL --compile_only --dump_bytecode --logs foo.log --progs $OUT
    echo $?
done
