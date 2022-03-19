#!/bin/bash
mvn sonar:sonar \
  -Dsonar.projectKey=telsos \
  -Dsonar.host.url=http://localhost:9000 \
  -Dsonar.login=bdc8817bfd02bc86424b72e8e92b3b9a4ce1a9c1
