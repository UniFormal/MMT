dir=`dirname $0`
java -Xmx8192m -cp $dir/lib/*:$dir/mmt/branches/informal/*:$dir/lfcatalog/*:$dir/mmt/* info.kwarc.mmt.api.frontend.Run -noshell file /var/data/localmh/MathHub/serve.msl
