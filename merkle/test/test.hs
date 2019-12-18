module Main where

import Test.Hspec

import qualified Test.Merkle

main :: IO ()
main = hspec
  Test.Merkle.spec
