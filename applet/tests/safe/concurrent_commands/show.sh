#!/bin/bash


PID=$(ps -C test_concurrent_commands -o pid=)
echo $PID
lsof -p $PID

PID=$(ps -C ashell_spawn_server -o pid=)
echo $PID
lsof -p $PID
