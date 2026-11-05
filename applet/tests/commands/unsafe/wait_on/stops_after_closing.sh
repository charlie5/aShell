#!/bin/bash

# Closes its output, stops itself, and is continued from outside. It exits with a failure,
# so that its status shows it reached the end, rather than being taken for ended while stopped.

exec >&- 2>&-
(sleep 0.5; kill -CONT $$) &
kill -STOP $$
exit 7
