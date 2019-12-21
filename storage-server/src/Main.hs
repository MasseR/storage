module Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment (Env (..))
import           Storage.Server      (server)

import           Storage.Logger      (logInfo, withLogger)

import           Data.Config

import           System.Environment  (getArgs)

main :: IO ()
main = withLogger $ \l -> do
  path <- listToMaybe <$> getArgs
  conf <- readConfig path
  let environment = Env { loggingEnv = l
                        , config = conf
                        }
  runAppM environment $ do
    logInfo "Starting up the server .."
    server
