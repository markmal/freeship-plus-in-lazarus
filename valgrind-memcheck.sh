#!/bin/bash

OPTS="--tool=memcheck"
OPTS="$OPTS --track-origins=yes"
OPTS="$OPTS --leak-check=full"
OPTS="$OPTS --log-file=valgrind.trc"
OPTS="$OPTS --num-callers=100"
OPTS="$OPTS --suppressions=valgrind-suppress.cfg"
OPTS="$OPTS --vgdb-error=0"

echo valgrind $OPTS $* ./FreeShip
valgrind $OPTS $* ./FreeShip & (sleep 2; vgdb --port=2345)
