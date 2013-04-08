dir=`dirname $0`
/usr/lib/jvm/java-7-openjdk-amd64/bin/java -Xmx2048m -cp $dir/lib/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run file $1
