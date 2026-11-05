#!/bin/bash

set -e

# This test requires a large stack size.

ulimit -s unlimited

# It starts as many processes as it is allowed. The usual limit can be a very large number, which could stop the machine
# from working. This limit is on all of this user's processes, so allow a few thousand more than are running now.

ulimit -u $(( $(ps -L -u "$(id -u)" --no-headers | wc -l) + 3000 ))

./test_max_processes