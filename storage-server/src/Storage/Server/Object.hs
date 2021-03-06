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
import           Control.Monad          (when)

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

import           Database
import           Database.Beam
import           Database.Object

import           Control.Lens           (from, toListOf, (^.))

type Reqs r m =
  ( MonadIO m
  , MonadUnliftIO m
  , WithLogging m
  , WithDB r m
  , HasPersistStore r
  )

listObjectsQ :: SqlSelect Sqlite Object
listObjectsQ = select $
  all_ (storage ^. objects)

lookupHash :: Hash -> SqlSelect Sqlite Object
lookupHash h = select $
  filter_ (\x -> x ^. objectId ==. val_ (HashKey h)) $ all_ (storage ^. objects)

writeHash :: Hash -> SqlInsert Sqlite ObjectT
writeHash h =
  insert (storage ^. objects) $ insertExpressions [ Object (val_ (HashKey h)) default_ ]

handler :: forall r m. Reqs r m => API (AsServerT m)
handler = API {..}
  where
    postObject :: Producer ByteString IO () -> m Hash
    postObject bs = do
      tree <- build (hoist liftIO bs)
      for_ tree $ \t -> withTransaction $ runDB $ do
        -- There doesn't seem to be "on conflict" style clause for sqlite in this version of beam
        -- Simulate it by checking for existence in a transaction
        let h = extract t
        exists <- runSelectReturningOne $ lookupHash h
        when (null exists) $
          runInsert $ writeHash h
      maybe (throwIO err500) (pure . extract) tree
    getObject :: Hash -> m (Producer ByteString IO ())
    getObject h = do
      x <- consume h
      maybe (throwIO err404) (\p -> withRunInIO $ \r -> pure (hoist r p)) x
    getObjects :: m [Hash]
    getObjects = do
      objs <- runDB (runSelectReturningList listObjectsQ)
      pure $ toListOf (traverse . objectId . from _HashKey) objs
