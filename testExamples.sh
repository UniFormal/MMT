#!/bin/bash
git clone http://gl.mathhub.info/MMT/urtheories.git
git clone http://gl.mathhub.info/MMT/examples.git
for i in urtheories examples; do rm -rf $i/{export,content,narration,relational}; done
for i in `seq 1 2`
do
   deploy/mmt.jar file urtheories/build.msl build
   deploy/mmt.jar file examples/build.msl build
done
cd examples
../deploy/mmt.jar file build.msl export
sbt compile
../deploy/mmt.jar file build.msl build
