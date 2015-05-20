dir=`dirname $0`
java -Xmx1024m -cp $dir/lib/*:$dir/lfcatalog/*:$dir/main/* info.kwarc.mmt.api.frontend.Run -file $1
