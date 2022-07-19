#!/bin/bash
mvn clean verify sonar:sonar \
  -Dsonar.projectKey=telsos \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.login=sqp_c4438f7e201d0f865bef896d57dbd6419e97df94
