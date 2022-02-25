#!/bin/bash


PID=$(ps -C ashell_spawn_server -o pid=)
echo $PID
lsof -p $PID
