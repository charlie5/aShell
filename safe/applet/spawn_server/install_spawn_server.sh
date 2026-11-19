#!/bin/bash

set -e

Here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# Find the 'ashell' project in this repository, unless it is already on the path.
#
export GPR_PROJECT_PATH=$GPR_PROJECT_PATH:$Here/../../../library

cd "$Here"
gprbuild -P spawn_server
sudo cp ashell_spawn_server /usr/bin

echo Done.
