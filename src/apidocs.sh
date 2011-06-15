#!/bin/sh

# Generate apidocs for all subprojects.

# Change to http://... for the server
BASE_URL=file:`pwd`
DEST=../deploy/apidocs/
# classpath libraries
LIBS=mmt-api/trunk/lib/tntbase-plugin-api.jar:../deploy/mmt-api/lib/mmt-api.jar:OWLMMT/lib/owlapi-bin.jar:../deploy/uom/lib/uom.jar:mmt-web/trunk/lib/commons-fileupload-1.2.1.jar:mmt-web/trunk/lib/joda-time-1.6.2.jar:mmt-web/trunk/lib/lift-actor_2.8.1-2.2-SNAPSHOT.jar:mmt-web/trunk/lib/lift-common_2.8.1-2.2-SNAPSHOT.jar:mmt-web/trunk/lib/lift-json_2.8.1-2.2-SNAPSHOT.jar:mmt-web/trunk/lib/lift-util_2.8.1-2.2-SNAPSHOT.jar:mmt-web/trunk/lib/lift-webkit_2.8.1-2.2-SNAPSHOT.jar:mmt-web/trunk/lib/log4j-1.2.16.jar:mmt-web/trunk/lib/servlet-api-2.5.jar:mmt-web/trunk/lib/slf4j-api-1.6.1.jar:mmt-web/trunk/lib/slf4j-log4j12-1.6.1.jar
# scala source directories
SRC=trunk/src/main
SOURCES="mmt-api/$SRC mmt-web/$SRC OWLMMT/src/ uom/$SRC"


mkdir -p $DEST
scaladoc -d $DEST -classpath $LIBS -doc-source-url $BASE_URL `find $SOURCES -name *.scala`
