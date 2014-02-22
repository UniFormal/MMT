rem This is the main extry point for running MMT on Windows.
rem Executing this script opens an interactive MMT shell.
rem MMT is "empty" by default, and the first thing to do is usually to issue a command like "file startup.msl" to load some configuration.

java -Xmx1024m -cp lib/*;mmt/*;/lfcatalog/lfcatalog.jar info.kwarc.mmt.api.frontend.Run log console ; %*
