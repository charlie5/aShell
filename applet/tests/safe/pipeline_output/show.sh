#!/bin/bash


PID=$(ps -C test_safe_pipeline_output -o pid=)
echo $PID
lsof -p $PID

#PID=$(ps -C ashell_spawn_server -o pid=)
#echo $PID
#lsof -p $PID
