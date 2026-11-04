#!/bin/bash

# Writes more error output than a pipe can hold, then runs for longer than any test should wait.

head -c 1000000 /dev/zero | tr '\0' x >&2
exec sleep 20
