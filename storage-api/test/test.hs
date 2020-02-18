module Main where

import           Test.Hspec

import qualified Test.Storage.API.System.Stats as Storage.API.System.Stats

main :: IO ()
main = hspec
  Storage.API.System.Stats.spec
