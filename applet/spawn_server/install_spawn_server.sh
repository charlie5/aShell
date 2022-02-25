#!/bin/bash

set -e

gprbuild -XOS=Linux -P spawn_server
sudo cp ashell_spawn_server /usr/bin

echo Done.
