{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Storage.Main where

import           MyPrelude

import           Control.Monad.App
import           Storage.Environment    (Env (..))
import           Storage.Server         (server)

import           Storage.Logger         (logInfo, withLogger)
import           Storage.Metrics        (newMetrics)
import           Storage.Metrics.Carbon (startCarbon)

import           Data.Config

import           System.Environment     (getArgs)
import           System.FilePath        ((</>))

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
      void startCarbon
      server
