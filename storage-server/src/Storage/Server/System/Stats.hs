{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage.Server.System.Stats where

import           MyPrelude

import           Storage.API.System.Stats (API (..), Counter (..), Gauge (..))

import           Servant.Server.Generic

import           Storage.Metrics          (HasMetrics, sample)

import qualified System.Metrics

type Reqs r m =
  ( MonadIO m
  , HasMetrics r
  , MonadReader r m
  )

handler :: forall r m. Reqs r m => API (AsServerT m)
handler = API {..}
  where
    counter :: Maybe System.Metrics.Value -> Counter
    counter = maybe (Counter 0) $ \case
      System.Metrics.Counter x -> Counter x
      _ -> Counter 0
    gauge :: Maybe System.Metrics.Value -> Gauge
    gauge = maybe (Gauge 0) $ \case
      System.Metrics.Gauge x -> Gauge x
      _ -> Gauge 0
    requests200 :: m Counter
    requests200 = counter <$> sample "wai.response_status_2xx"
    requests500 :: m Counter
    requests500 = counter <$> sample "wai.response_status_5xx"
    requests :: m Counter
    requests = counter <$> sample "wai.request_count"
    gcBytesAllocated :: m Counter
    gcBytesAllocated = counter <$> sample "rts.gc.bytes_allocated"
    gcMaxBytesUsed :: m Gauge
    gcMaxBytesUsed = gauge <$> sample "rts.gc.max_bytes_used"
    gcCurrentBytesUsed :: m Gauge
    gcCurrentBytesUsed = gauge <$> sample "rts.gc.current_bytes_used"
