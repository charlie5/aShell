#!/bin/bash


PID=$(ps -C test_spawn_server -o pid=)
echo $PID
lsof -p $PID

echo
echo
echo

PID=$(ps -C ashell_spawn_se -o pid=)
echo $PID
lsof -p $PID
