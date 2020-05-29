{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
module Storage.Metrics.Influxdb where

import           MyPrelude

import           Data.Aeson                        (FromJSON, ToJSON)

import           Storage.Metrics                   (HasMetrics (..), _Store)

import           Control.Lens                      (set, view)

import qualified Database.InfluxDB.Types           as I
import qualified Database.InfluxDB.Write           as IW
import qualified System.Remote.Monitoring.Influxdb as I

import           Data.GenValidity
import           Data.GenValidity.Text             ()

data Opts
  = Opts { host     :: Text
         , port     :: Int
         , database :: Text
         }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, Validity)

instance GenValid Opts where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

startInflux :: (HasMetrics r, MonadReader r m, MonadIO m) => Opts -> m ()
startInflux Opts{..} = do
  store <- view (metrics . _Store)
  let opts = I.defaultInfluxdbOptions writeParams
      writeParams = set IW.server server $
        IW.writeParams (I.Database database)
      server = I.Server host port False
  _ <- liftIO $ I.forkInfluxdb opts{I.prefix="storage"} store
  pure ()
