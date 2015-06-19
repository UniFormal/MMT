#!/bin/bash
rm -rf LATIN
git clone http://gl.mathhub.info/MMT/LATIN.git
for i in LATIN; do rm -rf $i/{export,content,narration,relational}; done
cd LATIN
for i in `seq 1 2`
do
   ../deploy/mmt.jar file build.msl build
done
../deploy/mmt.jar file build.msl build-mmt
../deploy/mmt.jar file build.msl literals
sbt compile
../deploy/mmt.jar file build.msl export
