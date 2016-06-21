#!/bin/sh
scalac -unchecked -d bin -classpath lib/scala-compiler.jar:lib/scala-library.jar:../../mmt-api/trunk/build/mmt-api.jar:bin src/main/info/kwarc/mmt/mizar/mizar/objects/*.scala src/main/info/kwarc/mmt/mizar/mizar/reader/*.scala src/main/info/kwarc/mmt/mizar/mmt/objects/*.scala src/main/info/kwarc/mmt/mizar/mizar/translator/*.scala src/main/info/kwarc/mmt/mizar/test/*.scala
