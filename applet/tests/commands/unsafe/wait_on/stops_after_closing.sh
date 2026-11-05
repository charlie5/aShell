#!/bin/bash

# Closes its output, stops itself, and is continued from outside. Only reaching
# the end gives a normal exit, rather than being taken for ended while stopped.

exec >&- 2>&-
(sleep 0.5; kill -CONT $$) &
kill -STOP $$
exit 0
