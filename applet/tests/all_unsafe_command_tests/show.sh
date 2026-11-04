#!/bin/bash


PID=$(ps -C test_all_unsafe_commands -o pid=)
echo $PID
lsof -p $PID
