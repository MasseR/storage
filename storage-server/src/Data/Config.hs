module Data.Config where

import           Data.Port                  (Port)
import           MyPrelude
import           Storage.Metrics.Carbon     (Carbon)
import           Storage.Persist            (PersistStore)

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Yaml.Config

import           Data.GenValidity
import           Data.GenValidity.Arbitrary

data Config
  = Config { port         :: !Port
           , persistStore :: !PersistStore
           , carbon       :: !Carbon
           }
  deriving Arbitrary via (ViaGenValid Config)

deriving stock instance Generic Config
deriving stock instance Show Config
deriving stock instance Eq Config
deriving instance ToJSON Config
deriving instance FromJSON Config
deriving instance Validity Config

instance GenValid Config where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

readConfig :: MonadIO m => Maybe FilePath -> m Config
readConfig path = liftIO $ loadYamlSettings [ fromMaybe "storage.yaml" path ] [] useEnv
