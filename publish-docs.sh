#!/bin/bash

set -e

myVer=${CI_TAG/#v/}
slick_dir=$(pwd)
docs_repo="slick/doc.git"
generated_docs=$slick_dir/doc/target/

mkdir -p /tmp/slick-docs/
cd /tmp/slick-docs/

echo "Cloning repo: $docs_repo"
rm -rf doc
git clone https://github.com/$docs_repo
cd doc

echo "Checking out to branch gh-pages"
git checkout gh-pages

mkdir "$myVer"
cd "$myVer"
echo "Copying generated docs to $myVer"
cp -r "$generated_docs"/* ./
cd ..

export GIT_SSH_COMMAND="ssh -i deploy-key "
git remote set-url origin git@github.com:$docs_repo
git add .
git commit -a -m "Add docs for tag release $myVer"
echo "Pushing to remote server"
git push
echo "Successfully pushed docs"
