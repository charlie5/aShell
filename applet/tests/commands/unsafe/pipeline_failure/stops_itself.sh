#!/bin/bash

# Stops itself, as if by Ctrl-Z, and carries on when resumed.

kill -STOP $$
echo Resumed
