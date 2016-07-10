#!/bin/sh

while true; do
  echo Compiling.
  ghc --make httpd.hs
  if [ $? != 0 ]; then
    read r
  else
    echo Starting.
    ./httpd -d $HOME/public_html
    echo Restart!
    sleep 1
  fi
done

