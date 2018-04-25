#!/bin/sh
set -e

mkdir -p /content/MathHub
java -jar /root/MMT/deploy/mmt.jar "$@"