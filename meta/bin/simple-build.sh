#!/bin/sh

for t in "$@"; do
  (cd $t &&
   runhaskell Setup.hs configure --user --prefix=$HOME &&
   runhaskell Setup.hs build &&
   runhaskell Setup.hs install --user)
done
