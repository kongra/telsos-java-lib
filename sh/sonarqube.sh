#!/bin/bash
mvn clean verify sonar:sonar \
  -Dsonar.projectKey=telsos \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.login=2492d8f584fff050c23bdebea4612306ae8da128
