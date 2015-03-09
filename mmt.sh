#!/bin/bash
dir=`dirname $0`
mvn -f $dir/pom.xml install
mvn -f $dir/pom.xml -pl mmt-tptp exec:exec \
  -Dexec.workingdir=$PWD \
  -Dexec.executable="java" \
  -Dexec.args="-cp %classpath info.kwarc.mmt.api.frontend.Run $*"
