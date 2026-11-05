#!/bin/bash

# Closes its output, and carries on for a while.

exec >&- 2>&-
exec sleep "${1:-1}"
