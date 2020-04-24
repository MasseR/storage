{-# LANGUAGE RecordWildCards            #-}
module Storage.Metrics.Carbon
  ( Opts(..)
  , startCarbon
  )
  where

import           MyPrelude


import           Control.Lens                    (view)

import           Storage.Metrics                 (HasMetrics, metrics, _Store)
import           System.Remote.Monitoring.Carbon (defaultCarbonOptions,
                                                  forkCarbon)
import qualified System.Remote.Monitoring.Carbon as C

import           Data.Aeson                      (FromJSON, ToJSON)

import           Data.GenValidity
import           Data.GenValidity.Text           ()

data Opts
  = Opts { host :: Text
         , port :: Int
         }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, Validity)

instance GenValid Opts where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- If this fails at some point, it will throw to parent and the parent will crash
startCarbon :: (HasMetrics r, MonadReader r m, MonadIO m) => Opts -> m ()
startCarbon Opts{..} = do
  store <- view (metrics . _Store)
  let opts = defaultCarbonOptions { C.host = host, C.port = port, C.prefix = "storage" }
  _ <- liftIO $ forkCarbon opts store
  pure ()
