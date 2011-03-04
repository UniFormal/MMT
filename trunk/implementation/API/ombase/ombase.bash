svn up
ant
java -jar ../lib/jetty-runner.jar --port 8080 ombase.war 2>&1 > jetty.log
