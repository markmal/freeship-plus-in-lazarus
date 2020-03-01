#!/bin/bash

OPTS="--tool=memcheck"
OPTS="$OPTS --track-origins=yes"
OPTS="$OPTS --leak-check=full"
OPTS="$OPTS --log-file=valgrind.trc"
OPTS="$OPTS --num-callers=100"
OPTS="$OPTS --suppressions=valgrind-suppress.cfg"

echo valgrind $OPTS $* ./FreeShip
valgrind $OPTS $* ./FreeShip
