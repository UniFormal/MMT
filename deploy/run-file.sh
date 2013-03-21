dir=`dirname $0`
java -Xmx2048m -cp $dir/lib/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run file $1
