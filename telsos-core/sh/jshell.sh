#!/bin/bash
MAVEN_OPTS="-server -Xmx1024m"
mvn compile com.github.johnpoth:jshell-maven-plugin:1.3:run
