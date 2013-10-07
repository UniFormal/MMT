rem This copies or deletes the files of the jEdit plugin to the jEdit settings directory
set dir=plugin/jars
java -cp %dir%/scala-library.jar;%dir%/mmt-api.jar;%dir%/MMTPlugin.jar info.kwarc.mmt.jeditsetup.Setup %*