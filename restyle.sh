#!/usr/bin/env bash

git diff --name-status origin/master \
  | grep .hs                         \
  | grep "^M"                        \
  | cut -f 2                         \
  | xargs stylish-haskell --inplace
