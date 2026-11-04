#!/bin/bash

# Catches an interrupt, then exits with a failure.

echo Ready
trap 'kill $Sleeper; exit 3' INT
sleep 20 &
Sleeper=$!
wait
