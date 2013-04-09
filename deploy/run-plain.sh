dir=`dirname $0`
if [ -e "/usr/lib/jvm/java-7-openjdk-amd64/bin/java" ]; then
  /usr/lib/jvm/java-7-openjdk-amd64/bin/java -Xmx2048m -cp $dir/lib/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run
else
    echo 'calling default java'
    java -Xmx2048m -cp $dir/lib/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run
fi

