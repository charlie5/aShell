#!/bin/bash

set -e

# This test requires a large stack size.

ulimit -s unlimited
./test_max_processes