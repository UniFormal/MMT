#!/bin/sh
scalac -d bin -deprecation -classpath ../lib/jomdoc.jar:bin src/mizar/objects/*.scala src/mizar/reader/*.scala src/mmt/objects/*.scala src/mizar/translator/*.scala src/test/*.scala
