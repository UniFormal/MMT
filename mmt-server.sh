#!/bin/bash
exec mvn -f /home/maeder/MMT/src/pom.xml -pl mmt-tptp exec:exec \
 -Dexec.executable=java \
 -Dexec.args="-cp %classpath info.kwarc.mmt.api.frontend.Run file /var/data/localmh/MathHub/meta/inf/config/OAF/oaf.msl"
