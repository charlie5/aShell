#!/bin/bash

# A pipeline stage with large error output, written before it reads any of its input.

head -c 300000 /dev/zero >&2
cat
