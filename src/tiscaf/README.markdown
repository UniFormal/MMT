tiscaf
======

Introduction
------------

tiscaf is an http server written in and intended to be used with the Scala programming language.

This project is based on the original work by Andrew Gaydenko (http://gaydenko.com/scala/tiscaf/httpd/) in its version 0.7. This explains why the software version starts at 0.8 in this repository.
Many thanks to him for his help and answers.

Motivations
-----------

Very often the current software exosphere makes it almost impossible to do things in a simple way. Almost all developers are under heavy pressure of dependency on J2EE and accompanying frameworks with dozens of external libraries (how many megabytes do those external jars take?). Any of you know these software monsters with infinite dependency trees... What if you need more freedom?

OTOH, if you like to spend your life dealing with extraneous-designed XML-files (rather than coding) and/or to satisfy multiple APIs exposing good ideas (even if they are indeed good), be warned - tiscaf lives in another world.

The server has just what you need to handle an HTTP request, no more. tiscaf treats the server as a low-level self-contained "assembly" module (being used instead of servlet engine) which is easy to embed into any application as well as to wrap with, say, own templating framework, any dispatching model (besides included helper for tree-like URIs space), this or that (besides default) execution environment (say, Comet-like), and so on.

Features
--------

 - nio - nio using permits resources managing.
 - dynamics - besides default execution pool it is possible to use own execution environment for each request handler.
 - full streaming - the server allows a streaming in both directions; say, you can download and upload ISO images.
 - dispatching - requests dispatching is limited with your imagination only.
 - multiport - listening to multiple ports is possible.
 - static content - file system and in-jar resources retrieving is supported out of the box (with files/directories browsing).
 - connections - persistent and SSL/TLS (experimental) connections are supported.
 - output - raw (content-length is known), buffered, gzipped (as a case of buffered), chunked - all modes are supported.
 - methods - POST/urlencoded, POST/multipart (with falling POST back to octets), GET and DELETE methods are supported.
 - sessions - via URI path extensions or cookies are supported.
 - config files - are absent.
 - depends on - nothing (again: nothing).
 - suspendable computation - HLet computation may be interrupted at any moment and resumed later
 - licensing - [LGPL](http://www.gnu.org/licenses/lgpl.html).

Documentation
-------------

 - the API documentation is available [here](http://gnieh.github.com/tiscaf/api/),
 - tutorials and examples may be found in the wiki pages.
