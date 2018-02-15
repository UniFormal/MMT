@echo off
rem This is the main extry point for running MMT on Windows.

java -Xmx1024m -cp %~dp0/mmt.jar info.kwarc.mmt.api.frontend.Run %*
