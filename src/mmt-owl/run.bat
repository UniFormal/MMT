: the MMT Shell is invoked by java -cp scala/*; mmt-api.jar info.kwarc.mmt.api.RunNull 
: Xmx1024m increases the memory available
: -cp Java classpath
: jar files of the scala library are here ../../deploy/lib/*

: command line paramets are passed to the shell and executed as a command
java -Xmx1024m -cp ../mmt-api/trunk/build/mmt-api.jar;../../deploy/lib/*;../../deploy/lfcatalog/*;../../deploy/mmt/*;mmt-owl.jar;lib/owlapi-bin.jar info.kwarc.mmt.api.frontend.RunNull



