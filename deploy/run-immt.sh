dir=`dirname $0`
java -Xmx1024m -cp $dir/lib/*:$dir/mmt/branches/informal/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run file $1
