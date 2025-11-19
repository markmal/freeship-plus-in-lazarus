#!/bin/bash

OPTS="--tool=memcheck"
OPTS="$OPTS --track-origins=yes"
OPTS="$OPTS --keep-stacktraces=alloc-and-free"
OPTS="$OPTS --error-limit=no"
OPTS="$OPTS --keep-debuginfo=yes"
OPTS="$OPTS --read-inline-info=yes"
OPTS="$OPTS --read-var-info=yes"
OPTS="$OPTS --leak-check=full"
OPTS="$OPTS --show-leak-kinds=all"
OPTS="$OPTS --log-file=valgrind.trc"
OPTS="$OPTS --num-callers=100"
OPTS="$OPTS --suppressions=valgrind-suppress.cfg"
OPTS="$OPTS --vgdb-error=0"
OPTS="$OPTS --vgdb=full"
OPTS="$OPTS -v"

echo valgrind $OPTS $* ./FreeShip
valgrind $OPTS $* ./FreeShip & (sleep 2; vgdb --port=2345)
