{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Storage.Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment   (Env (..))
import           Storage.Server        (server)

import           Storage.Logger        (logInfo, withLogger)
import           Storage.Metrics       (newMetrics)
import           Storage.Metrics.Push  (startMetricsPush)

import           Data.Config           (readConfig)

import           Control.Lens          (view)
import           Data.Generics.Product (field)

import           System.Environment    (getArgs)

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
    traverse_ startMetricsPush (view (field @"config" . field @"metrics") environment)
    server
