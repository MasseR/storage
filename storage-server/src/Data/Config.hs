module Data.Config where

import           Data.Config.Metrics        (Metrics)
import           Data.Port                  (Port)
import           MyPrelude
import           Storage.Persist            (PersistStore)

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Yaml.Config

import           Control.Lens               (Lens', lens, set, view)

import           Data.GenValidity
import           Data.GenValidity.Arbitrary

data Config
  = Config { port         :: !Port
           , persistStore :: !PersistStore
           , dataDir      :: FilePath
           , metrics      :: !(Maybe Metrics)
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

class HasConfig a where
  {-# MINIMAL getConfig, setConfig | config #-}
  getConfig :: a -> Config
  getConfig = view config

  setConfig :: a -> Config -> a
  setConfig = flip (set config)

  config :: Lens' a Config
  config = lens getConfig setConfig


readConfig :: MonadIO m => Maybe FilePath -> m Config
readConfig path = liftIO $ loadYamlSettings [ fromMaybe "storage.yaml" path ] [] useEnv
