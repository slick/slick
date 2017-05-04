#!/bin/bash

# Setup:
# - Copy keys to admin/secring.asc, admin/pubring.asc
#   - Use a new key pair per repository
# - cp ~/.sbt/sonatype.sbt ./
# - tar cvf secrets.tar admin/secring.asc sonatype.sbt
# - travis encrypt-file secrets.tar --add
# - rm sonatype.sbt admin/secring.asc secrets.tar
# - Set passphrase in admin/publish-settings.sbt

set -e

if [[ "$TRAVIS_TAG" =~ ^v[0-9]+\.[0-9]+(\.[0-9]+)?(-[A-Za-z0-9-]+)? ]]; then
  echo "Going to release from tag $TRAVIS_TAG!"
  myVer=$(echo $TRAVIS_TAG | sed -e s/^v//)
  publishVersion='set every version := "'$myVer'"'
  extraTarget="+publishSigned"
  cp admin/publish-settings.sbt ./
  tar xf secrets.tar
fi

sbt -jvm-opts jvmopts.travis -Dslick.testkit-config=test-dbs/testkit.travis.conf "$publishVersion" +testAll $extraTarget
