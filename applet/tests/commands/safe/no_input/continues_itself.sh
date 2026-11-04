#!/bin/bash

# Stops itself, is continued from outside, then runs a little longer.

(sleep 0.3; kill -CONT $$) &
kill -STOP $$
sleep 0.5
