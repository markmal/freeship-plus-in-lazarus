#!/bin/bash

OPTS="--tool=callgrind"
OPTS="$OPTS --dump-instr=yes"
OPTS="$OPTS --simulate-cache=yes"
OPTS="$OPTS --collect-jumps=yes"
OPTS="$OPTS --log-file=callgrind.trc"

echo valgrind $OPTS $* ./FreeShip

echo At the moment you want actual profiling to start, run
echo callgrind_control -i on

valgrind $OPTS $* ./FreeShip
echo use kcachegrind with last callgrind.out.
