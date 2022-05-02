#!/bin/sh
set -e

# Ensure that the directory for content exists
mkdir -p /content/MathHub

# if the $MMT_ARCHIVES variable is set, install all the archives
if [ ! -z "$MMT_ARCHIVES" ]; then
    echo "Installing MMT Archives: $MMT_ARCHIVES"
    mmt "$@" --file /install.msl "lmh install $MMT_ARCHIVES ; exit"
    echo "Done. "
fi

# And start MMT
mmt "$@"