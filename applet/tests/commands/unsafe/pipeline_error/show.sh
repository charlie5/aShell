#!/bin/bash


PID=$(ps -C test_pipeline_error -o pid=)
echo $PID
lsof -p $PID
