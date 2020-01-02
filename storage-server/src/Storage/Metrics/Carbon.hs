{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Storage.Metrics.Carbon
  ( Carbon
  , HasCarbon(..)
  , startCarbon
  )
  where

import           MyPrelude

import           Data.Traversable                (for)

import           Control.Lens                    (Lens', lens, view)

import           Storage.Metrics                 (HasMetrics, metrics, _Store)
import           System.Remote.Monitoring.Carbon (defaultCarbonOptions,
                                                  forkCarbon)
import qualified System.Remote.Monitoring.Carbon as C

import           Control.Concurrent              (killThread)

import           Data.Aeson                      (FromJSON, ToJSON)

data Opts
  = Opts { host :: Text
         , port :: Int
         }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Carbon = Carbon ( Maybe Opts ) deriving newtype (ToJSON, FromJSON)

class HasCarbon a where
  getCarbon :: a -> Carbon
  setCarbon :: a -> Carbon -> a

carbon :: HasCarbon a => Lens' a Carbon
carbon = lens getCarbon setCarbon

type CloseCarbon = IO ()

-- If this fails at some point, it will throw to parent and the parent will crash
startCarbon :: (HasMetrics r, MonadReader r m, HasCarbon r, MonadIO m) => m CloseCarbon
startCarbon = do
  Carbon o <- view carbon
  store <- view (metrics . _Store)
  close <- for o $ \Opts{..} -> do
    let opts = defaultCarbonOptions { C.host = host, C.port = port, C.prefix = "storage" }
    tid <- liftIO $ forkCarbon opts store
    let kill = killThread tid
    pure kill
  pure $ fromMaybe (pure ()) close
