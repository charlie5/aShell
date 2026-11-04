#!/bin/bash

# Writes a line of error output, naming this command in a pipeline.

echo "${1:-First}" >&2
