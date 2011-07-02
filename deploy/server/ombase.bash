#!/bin/bash
java -jar ../lib/jetty-runner.jar --port 8080 lib/mmt-web.war 2>&1 > jetty.log
