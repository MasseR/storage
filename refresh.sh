#!/usr/bin/env bash

while :; do
  find */{*.cabal,src,test,app} -type f | entr -r -d -c -s "cabal test $1 --test-show-details=direct"
done
