#!/bin/bash


PID=$(ps -C test_concurrent_pipelines -o pid=)
echo $PID
lsof -p $PID
