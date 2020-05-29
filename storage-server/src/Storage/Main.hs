{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Storage.Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment    (Env (..))
import           Storage.Server         (server)

import           Storage.Logger         (logInfo, withLogger)
import           Storage.Metrics        (newMetrics)
import           Storage.Metrics.Push   (startMetricsPush)

import           Data.Config            (Config (..), readConfig)

import           Control.Lens           (view)
import           Data.Generics.Product  (field)
import           System.FilePath        ((</>))

import           System.Environment     (getArgs)

import           Database.SQLite.Simple (withConnection)

withEnv :: Config -> (Env -> IO a) -> IO a
withEnv config@Config{dataDir} f =
  withConnection (dataDir </> "storage.sqlite") $ \connection ->
  withLogger $ \loggingEnv -> do
    metrics <- newMetrics
    let environment = Env { .. }
    f environment


defaultMain :: IO ()
defaultMain = do
  path <- listToMaybe <$> getArgs
  config <- readConfig path
  withEnv config $ \environment ->
    runAppM environment $ do
      logInfo "Starting up the server .."
      traverse_ startMetricsPush (view (field @"config" . field @"metrics") environment)
      server
