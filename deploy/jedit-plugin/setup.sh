#!/bin/bash
dir=`dirname $0`/plugin/jars
java -cp $dir/scala-library.jar:$dir/mmt-api.jar:$dir/MMTPlugin.jar info.kwarc.mmt.jeditsetup.Setup "$@"

plug="$2/plugins/info.kwarc.mmt.jedit.MMTPlugin" 
mar="${plug}/mars/urtheories.mar"

if [ "$1" == "install" ]
then
  echo "extracting ${mar}"
  unzip -d "${plug}/mars" "${mar}"
else
  echo "deleting ${plug}/"
  rm -rf "${plug}"
fi
