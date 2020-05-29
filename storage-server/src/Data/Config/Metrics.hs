module Data.Config.Metrics where

import           MyPrelude

import           Data.Aeson

import qualified Storage.Metrics.Carbon     as C
import qualified Storage.Metrics.Influxdb   as I

import           Data.GenValidity
import           Data.GenValidity.Arbitrary

data Metrics = Carbon C.Opts | Influx I.Opts
  deriving (Show, Eq, Generic)
  deriving Arbitrary via (ViaGenValid Metrics)

instance Validity Metrics

instance GenValid Metrics where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance ToJSON Metrics where
  toJSON (Carbon c) = object [ "carbon" .= toJSON c ]
  toJSON (Influx c) = object [ "influx" .= toJSON c ]

instance FromJSON Metrics where
  parseJSON = withObject "Metrics" $ \v ->
    Carbon <$> v .: "carbon" <|> Influx <$> v .: "influx"
