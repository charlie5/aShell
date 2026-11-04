#!/bin/bash


PID=$(ps -C test_all_safe_commands -o pid=)
echo $PID
lsof -p $PID
