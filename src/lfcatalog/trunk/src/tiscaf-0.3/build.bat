rem @echo off
mkdir classes

scalac -d myclasses src\zgs\httpd\*.scala src\zgs\httpd\let\*.scala src\zgs\utl\*.scala src\zgs\sync\*.scala
