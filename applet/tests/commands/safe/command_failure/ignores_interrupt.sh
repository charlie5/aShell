#!/bin/bash
# Carries on after an interrupt, so that interrupting it must not hold up other commands.
trap "" INT
sleep 3
