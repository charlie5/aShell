#!/bin/bash

# Catches an interrupt, then exits with a failure.

echo Ready
trap "exit 3" INT
sleep 20 &
wait
