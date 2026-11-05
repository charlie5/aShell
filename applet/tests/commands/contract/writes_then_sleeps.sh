#!/bin/bash

# Leaves some errors in its pipe, then carries on until it is stopped.

echo Early >&2
exec sleep 20
