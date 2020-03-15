#!/bin/bash

OPTS="--tool=callgrind"
OPTS="$OPTS --dump-instr=yes"
OPTS="$OPTS --simulate-cache=yes"
OPTS="$OPTS --collect-jumps=yes"
OPTS="$OPTS --log-file=callgrind.trc"

echo valgrind $OPTS $* ./FreeShip
valgrind $OPTS $* ./FreeShip
