module Storage.Metrics
  ( Metrics
  , HasMetrics(..)
  , metrics
  , _Store
  , metricsMiddleware
  , newMetrics
  , sample
  )
  where

import           Control.Lens        (Lens', ix, lens, preview, view)
import           System.Metrics      (Store, Value, newStore, registerGcMetrics,
                                      sampleAll)

import           Network.Wai         (Middleware)
import qualified Network.Wai.Metrics as Wai

import           MyPrelude

newtype Metrics = Metrics { getStore :: Store }

class HasMetrics a where
  getMetrics :: a -> Metrics
  setMetrics :: a -> Metrics -> a

metrics :: HasMetrics a => Lens' a Metrics
metrics = lens getMetrics setMetrics

_Store :: Lens' Metrics Store
_Store = lens getStore (\_ s -> Metrics s)

sample :: (HasMetrics r, MonadReader r m, MonadIO m)  => Text -> m (Maybe Value)
sample k = do
  m <- view (metrics . _Store) >>= liftIO . sampleAll
  pure $ preview (ix k) m

metricsMiddleware :: (HasMetrics r, MonadReader r m, MonadIO m) => m Middleware
metricsMiddleware = do
  m <- view (metrics . _Store) >>= liftIO . Wai.registerWaiMetrics
  pure (Wai.metrics m)

newMetrics :: MonadIO m => m Metrics
newMetrics = liftIO $ do
  s <- newStore
  registerGcMetrics s
  pure $ Metrics s
