rem This is like run.bat except that it automatically loads an msl file passed as an argument.
rem You may want to associate the msl file ending with this script.

java -Xmx1024m -cp %~dp0/lib/*;%~dp0/mmt/*;%~dp0/lfcatalog/* info.kwarc.mmt.api.frontend.Run file %1
