#!/bin/bash
mvn clean package
cp target/telsos.war /media/kongra/Devel/Javasoft/apache-tomee-plume-9.0.0-M7/webapps
