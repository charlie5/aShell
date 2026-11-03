#!/bin/bash

set -E

for i in {1..20}; do
   echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   echo EPOCH: $i
   echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   echo
   sleep 0.5
   ./test_concurrent_pipelines
done

echo Done !.
