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

import           Control.Retry

import           Data.Config           (readConfig)

import           Control.Lens          (view)
import           Data.Generics.Product (field)

import           System.Environment    (getArgs)

import           Control.Monad.Catch   (Handler (..))

import Network.HTTP.Client (HttpException)

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
    traverse_ (withRetry . startMetricsPush) (view (field @"config" . field @"metrics") environment)
    server
  where
    limitedBackoff = exponentialBackoff 50 <> limitRetries 5
    withRetry f = recovering limitedBackoff [\_st -> Handler (const @_ @HttpException (pure True))] (\_st -> f)
