module Main where

import Test.Hspec

import qualified Test.Merkle
import qualified Test.Merkle.Hash

main :: IO ()
main = hspec $ do
  Test.Merkle.spec
  Test.Merkle.Hash.spec
