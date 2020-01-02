{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Storage.Client
  (
  -- * API
    postObject
  , getObject
  -- * Setup
  , StorageEnv(..)
  , HasStorageEnv
  , storageEnv
  -- * Runners
  , runClient
  , withClient
  )
  where

import           Storage.API
import qualified Storage.API.Object       as Object

import           Control.Lens             (Lens', lens, view)

import           Network.HTTP.Client      (Manager)
import           Servant.API.Generic      (fromServant)
import           Servant.Client.Generic
import           Servant.Client.Streaming

import           MyPrelude


import           Pipes

import           Data.Merkle.Hash

import Control.DeepSeq (NFData)

data StorageEnv
  = StorageEnv { manager :: !Manager
               , base    :: !BaseUrl
               }

class HasStorageEnv a where
  getStorageEnv :: a -> StorageEnv
  setStorageEnv :: a -> StorageEnv -> a

storageEnv :: HasStorageEnv a => Lens' a StorageEnv
storageEnv = lens getStorageEnv setStorageEnv

-- | Run the client to get the result
--
-- Note that this will force the result in memory first, so not ideal for streaming
runClient :: (NFData a, HasStorageEnv r, MonadReader r m, MonadIO m) => ClientM a -> m (Either ClientError a)
runClient c = do
  StorageEnv{..} <- view storageEnv
  let env = mkClientEnv manager base
  liftIO $ runClientM c env

-- | Run the client within a callback
withClient :: (MonadUnliftIO m, HasStorageEnv r, MonadReader r m, MonadIO m) => ClientM a -> (Either ClientError a -> m b) -> m b
withClient c f = do
  StorageEnv{..} <- view storageEnv
  let env = mkClientEnv manager base
  withRunInIO $ \run -> withClientM c env (run . f)

-- | Post a raw object to the storage
postObject :: Producer ByteString IO () -> ClientM Hash

-- | Get a raw object from the storage
getObject :: Hash -> ClientM (Producer ByteString IO ())

API{ index = _
   , system = _
   , object = fromServant @_ @(AsClientT ClientM) ->
      Object.API { post = postObject
                 , get = getObject
                 }
   } = genericClient
