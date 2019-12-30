module Data.Config where

import           Data.Port        (Port)
import           MyPrelude
import           Storage.Persist  (PersistStore)

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Yaml.Config

data Config
  = Config { port         :: Port
           , persistStore :: PersistStore
           }

deriving stock instance Generic Config
deriving instance ToJSON Config
deriving instance FromJSON Config

readConfig :: MonadIO m => Maybe FilePath -> m Config
readConfig path = liftIO $ loadYamlSettings [ fromMaybe "storage.yaml" path ] [] useEnv
