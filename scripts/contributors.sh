#!/bin/sh

if [ $1 ]; then

  if [ $2 ]; then
    HEAD=$2
  else
    HEAD=head
  fi
  git log '--format=format:* [``%h``](https://github.com/slick/slick/commit/%H) %s' --merges $1..$HEAD | grep -v "from slick/tmp" | sed -e 's/.* from //' | grep -v "Merge branch" | sed -e 's|/.*||' | sort | uniq | sed -e 's|\(.*\)|, [\1](https://github.com/\1)|'

else
  echo "Wrong argsments:"
  echo "Usage: "
  echo "  ./contributors.sh v3.2.3 head"
  echo "  ./contributors.sh v3.2.3 3.3.0"
  echo "  ./contributors.sh v3.2.3"
fi