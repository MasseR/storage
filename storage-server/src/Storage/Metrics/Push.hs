{-# LANGUAGE LambdaCase #-}
module Storage.Metrics.Push where

import           MyPrelude

import           Storage.Metrics          (HasMetrics)
import qualified Storage.Metrics.Carbon   as C
import qualified Storage.Metrics.Influxdb as I

import           Data.Config.Metrics

startMetricsPush :: (HasMetrics r, MonadReader r m, MonadUnliftIO m) => Metrics -> m ()
startMetricsPush = \case
  Carbon opts -> C.startCarbon opts
  Influx opts -> I.startInflux opts
