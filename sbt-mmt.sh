#!/bin/bash
script="$(readlink -f $0)"
dir="$(dirname $script)"

# relative filename arguments will not work
# and also spaces in filenames are not supported.
cd $dir
exec sbt -ivy /tmp/.ivy2 "mmt/run $*"
