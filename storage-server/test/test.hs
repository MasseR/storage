module Main where

import           Test.Hspec

import qualified Test.Config as Config
import qualified Test.Server as Server

main :: IO ()
main = hspec $ do
  Server.spec
  Config.spec
