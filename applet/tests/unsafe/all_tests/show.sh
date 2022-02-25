#!/bin/bash


PID=$(ps -C test_all -o pid=)
echo $PID
lsof -p $PID
