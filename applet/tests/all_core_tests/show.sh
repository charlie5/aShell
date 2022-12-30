#!/bin/bash


PID=$(ps -C test_all_core -o pid=)
echo $PID
lsof -p $PID
