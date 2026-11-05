#!/bin/bash

# Closes its output so that the parent's wait blocks, then interrupts the parent during that wait. Only reaching the end gives a normal exit.

exec >&- 2>&-
sleep 0.3
kill -INT $PPID
sleep 0.3
