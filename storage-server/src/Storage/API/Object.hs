{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Storage.API.Object
  ( API
  , handler
  , Reqs
  )
  where

import           Servant.API
import           Servant.API.Generic
import           Servant.Pipes          ()
import           Servant.Server.Generic

import           Servant.Server         (err500)
import           UnliftIO.Exception     (throwIO)

import           MyPrelude

import           Storage                (build)
import           Storage.Persist        (HasPersistStore)

import           Pipes
-- import qualified Pipes.Prelude          as P

import           Storage.Logger

import           Data.Merkle.Hash

import           Control.Comonad        (extract)

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , MonadUnliftIO m
  , WithLogging m
  , HasPersistStore r
  )

newtype API route
  = API { post :: route :- StreamBody NoFraming OctetStream (Producer ByteString IO ()) :> Post '[JSON] Hash }
  deriving stock (Generic)


handler :: forall r m. Reqs r m => API (AsServerT m)
handler = API {..}
  where
    post :: Producer ByteString IO () -> m Hash
    post bs = do
      tree <- build (hoist liftIO bs)
      logInfo $ tshow $ fmap length tree
      maybe (throwIO err500) (pure . extract) tree
