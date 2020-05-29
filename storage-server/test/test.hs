module Main where

import           Test.Hspec

import qualified Test.Config           as Config
import qualified Test.Config.Metrics   as Config.Metrics
import qualified Test.Database.Migrate as Database.Migrate
import qualified Test.Server           as Server

main :: IO ()
main = hspec $ do
  Config.Metrics.spec
  Config.spec
  Database.Migrate.spec
  Server.spec
