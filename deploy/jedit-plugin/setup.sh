#!/bin/bash
dir=`dirname $0`/plugin/jars
java -cp $dir/scala-library.jar:$dir/mmt-api.jar:$dir/MMTPlugin.jar info.kwarc.mmt.jeditsetup.Setup "$@"
