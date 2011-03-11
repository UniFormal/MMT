#!/bin/sh

# usage: setup_project.sh project_name

mkdir $1
mkdir $1/trunk $1/releases $1/branches
svn add $1
