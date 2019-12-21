module Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment (Env (..))
import           Storage.Server      (server)

import           Storage.Logger      (logInfo, withLogger)

import           Data.Config

main :: IO ()
main = withLogger $ \l -> do
  conf <- readConfig Nothing
  let environment = Env { loggingEnv = l
                        , config = conf
                        }
  runAppM environment $ do
    logInfo "Starting up the server .."
    server
