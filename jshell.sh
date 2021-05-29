#!/bin/bash
export JAVA_HOME=/media/kongra/Devel/Javasoft/jdk-16.0.1
export CLASSPATH=$(mvn dependency:build-classpath | grep -vE "[INFO]|[ERROR]")
mvn jshell:run
