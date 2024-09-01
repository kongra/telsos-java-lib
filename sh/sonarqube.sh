#!/bin/bash
mvn clean verify sonar:sonar \
  -Dsonar.projectKey=telsos \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.login=<sonarqube-token>
