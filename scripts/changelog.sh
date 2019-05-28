#!/bin/sh

if [ $1 ]; then

  if [ $2 ]; then
    HEAD=$2
  else
    HEAD=head
  fi
  git log '--format=format:* [``%h``](https://github.com/slick/slick/commit/%H) %s' --no-merges $1..$HEAD

else
  echo "Wrong arguments:"
  echo "Usage: "
  echo "  ./changelog.sh v3.2.3 head"
  echo "  ./changelog.sh v3.2.3 3.3.0"
  echo "  ./changelog.sh v3.2.3"
fi