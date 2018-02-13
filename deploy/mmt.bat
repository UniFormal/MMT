@echo off
rem This is the main extry point for running MMT on Windows.

java -Xmx2G -cp %~dp0/lib/*;%~dp0/mmt.jar;%~dp0/lfcatalog/lfcatalog.jar info.kwarc.mmt.api.frontend.Run %*
