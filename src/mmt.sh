#!/bin/bash
dir=`dirname $0`
mvn -f $dir/pom.xml install
mvn -f $dir/pom.xml -pl mmt-tptp exec:java -Dexec.mainClass=info.kwarc.mmt.api.frontend.Run -Dexec.args="$*"
