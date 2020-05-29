{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage.Server.Object
  ( handler
  , Reqs
  )
  where

import           Control.Comonad        (extract)

import           MyPrelude

import           Data.Merkle.Hash

import           Pipes

import           Storage                (build, consume)
import           Storage.Persist        (HasPersistStore)

import           Servant.Pipes          ()
import           Servant.Server.Generic

import           Servant.Server         (err404, err500)
import           UnliftIO.Exception     (throwIO)

import           Storage.Logger

import           Storage.API.Object     (API (..))

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , MonadUnliftIO m
  , WithLogging m
  , HasPersistStore r
  )

handler :: forall r m. Reqs r m => API (AsServerT m)
handler = API {..}
  where
    postObject :: Producer ByteString IO () -> m Hash
    postObject bs = do
      tree <- build (hoist liftIO bs)
      maybe (throwIO err500) (pure . extract) tree
    getObject :: Hash -> m (Producer ByteString IO ())
    getObject h = do
      x <- consume h
      maybe (throwIO err404) (\p -> withRunInIO $ \r -> pure (hoist r p)) x
