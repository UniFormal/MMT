#!/usr/bin/env bash
mvn install:install-file -Dfile=lib/Console.jar -DgroupId=console -DartifactId=Console -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=lib/ErrorList.jar -DgroupId=errorlist -DartifactId=ErrorList -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=lib/Hyperlinks.jar -DgroupId=gatchan.jedit -DartifactId=hyperlinks -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=lib/jedit.jar -DgroupId=org.gjt.sp -DartifactId=jedit -Dversion=1.0 -Dpackaging=jar
mvn install:install-file -Dfile=lib/SideKick.jar -DgroupId=sidekick -DartifactId=SideKick -Dversion=1.0 -Dpackaging=jar
