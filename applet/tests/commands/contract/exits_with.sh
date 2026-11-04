#!/bin/bash

# Exits with the given status, after explaining why.

echo "Exiting with $1" >&2
exit "$1"
