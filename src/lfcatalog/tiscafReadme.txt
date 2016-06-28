tiscaf

tiscaf - TIny SCAla Framework - is a framework I'm working on. The 
framework is written in and intended to be used with Scala programming
language (http://www.scala-lang.org/). Here a part of the framework
is presented - it is http server:

    * nio - nio using permits resources managing.
    * full streaming - the server allows a streaming in both direction,
      say, you can download and upload ISO images.
    * dispatching - requests dispatching is limited with your imagination
      only.
    * multiport - listening to multiple ports is possible.
    * static content - file system resources retrieving is supported out
      of the box (with files/directories browsing).
    * connections - persistent connections are also supported.
    * output - raw (content-length is known), buffered, gzipped (as a 
      case of buffered), chunked - all cases are supported.
    * methods - GET, POST/urlencoded and POST/multipart are supported.
    * sessions - via URI path extensions and/or cookies are supported.
    * config files - are absent.
    * codebase - ~80KB.
    * depends on - nothing.
    * licensing - LGPL.


homepage: http://gaydenko.com/scala/tiscaf/httpd/

licensing: LGPL, see COPYING file.

to compile: ./build.sh

to start/stop demo server on port 8910: ./startHomeServer.sh or ./stopHomeServer.sh

for windows users appropriate .bat files are provided.

author: Andrew Gaydenko, a@gaydenko.com (please, add 'tiscaf' to your subject)
