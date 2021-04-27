{-# LANGUAGE RecordWildCards #-}
module Storage.Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment    (Env (..))
import           Storage.Server         (server)

import           Storage.Logger         (logInfo, withLogger)
import           Storage.Metrics        (newMetrics)

import           Data.Config

import           System.Environment     (getArgs)

withEnv :: (Env -> IO a) -> IO a
withEnv f = withLogger $ \loggingEnv -> do
  path <- listToMaybe <$> getArgs
  config <- readConfig path
  metrics <- newMetrics
  let environment = Env { .. }
  f environment


defaultMain :: IO ()
defaultMain = withEnv $ \environment ->
  runAppM environment $ do
    logInfo "Starting up the server .."
    server
