#!/bin/sh

# usage: setup_project.sh project_name

mkdir $1
mkdir -p $1/trunk $1/releases $1/branches $1/trunk/src/main/info/kwarc/mmt/ $1/trunk/src/test/info/kwarc/mmt/ $1/trunk/lib
svn add $1
