dir=`dirname $0`
java -Xmx3072m -cp $dir/lib/*:$dir/mmt/branches/informal/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run -noshell file /var/data/localmh/MathHub/build.msl
