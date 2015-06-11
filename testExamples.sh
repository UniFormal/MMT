#!/bin/bash
git clone http://gl.mathhub.info/MMT/urtheories.git
git clone http://gl.mathhub.info/MMT/examples.git
for i in urtheories examples; do rm -rf $i/{content,narration,relational}; done
for i in `seq 1 3`
do
   deploy/mmt file urtheories/build.msl build
   deploy/mmt file examples/build.msl build
done
mv deploy/mmt deploy/mmt.jar
cd examples
../deploy/mmt.jar file build.msl export
sbt compile
for i in `seq 1 2`; do ../deploy/mmt.jar file build.msl build; done
