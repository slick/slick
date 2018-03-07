#!/bin/bash

# Setup:
# - Copy keys to admin/secring.asc, admin/pubring.asc
#   - Use a new key pair per repository
# - ssh-keygen -t rsa -b 4096 -C "<github-user-email>" #save as deploykey
#   - Setup github deploy keys using generated keys, keep a copy of generated keys
# - cp deploykey deploykey.pub admin/
# - openssl rsa -in admin/deploykey -outform pem > admin/deploykey.pem
# - chmod 400 admin/deploykey.pem
# - cp ~/.sbt/sonatype.sbt ./
# - tar cvf secrets.tar admin/secring.asc sonatype.sbt admin/deploykey.pem
# - travis encrypt-file secrets.tar --add
# - rm sonatype.sbt admin/secring.asc admin/deploykey admin/deploykey.pub admin/deploykey.pem secrets.tar
# - Set passphrase in admin/publish-settings.sbt
set -e

if [[ "$TRAVIS_TAG" =~ ^v[0-9]+\.[0-9]+(\.[0-9]+)?(-[A-Za-z0-9-]+)? ]]; then
  echo "Going to release from tag $TRAVIS_TAG!"
  openssl aes-256-cbc -K $encrypted_c3c0a1170361_key -iv $encrypted_c3c0a1170361_iv -in secrets.tar.enc -out secrets.tar -d
  myVer=$(echo $TRAVIS_TAG | sed -e s/^v//)
  publishVersion='set every version := "'$myVer'"'
  extraTarget="+publishSigned makeSite"
  publish_docs=1
  cp admin/publish-settings.sbt ./
  echo "Contents of secrets.tar:"
  tar tvf secrets.tar
  tar xf secrets.tar
  chmod 400 admin/deploykey.pem
  echo "sbt build files:"
  ls -l *.sbt project/*.scala
fi

sbt -jvm-opts jvmopts.travis -Dslick.testkit-config=test-dbs/testkit.travis.conf "$publishVersion" +testAll $extraTarget

if [ "$publish_docs" -eq "1" ]; then
  slick_dir=$(pwd)
  docs_repo="slick/doc.git"
  generated_docs=$slick_dir/doc/target/
  mkdir /tmp/slick-docs/
  cd /tmp/slick-docs/
  echo "Cloning repo: $docs_repo"
  git clone https://github.com/$docs_repo
  cd doc
  echo "Checking out to branch gh-pages"
  git checkout gh-pages
  mkdir $myVer
  cd $myVer
  echo "Copying generated docs to $myVer"
  cp -r $generated_docs/* ./
  cd ..
  export GIT_SSH_COMMAND="ssh -i $slick_dir/admin/deploykey.pem "
  git remote set-url origin git@github.com:$docs_repo
  git add .
  git commit -a -m "Add docs for tag release $myVer"
  echo "Pushing to remote server"
  git push
  echo "Successfully pushed docs"
fi
