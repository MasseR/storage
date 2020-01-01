{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Storage.Server where

import           MyPrelude
import qualified Storage.API                as API

import           Control.Lens               (view)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           Data.Port

import qualified Storage.Server.Object     as Object

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , Object.Reqs r m
  )

handler :: Reqs r m => API.API (AsServerT m)
handler =
  API.API { .. }
  where
    index = pure "hello"
    object = toServant Object.handler

api :: Proxy (ToServantApi API.API)
api = genericApi @API.API Proxy

type Constraint r m = (Reqs r m, MonadReader r m, HasPort r, MonadIO m)

app :: forall r m. Constraint r m => (forall x. m x -> Servant.Handler x) -> Application
app nat =
  serve api (hoistServer api nat (genericServerT handler))

server :: (MonadUnliftIO m, Constraint r m) => m ()
server = do
  Port p <- view port
  withRunInIO $ \runInIO ->
    run (fromIntegral p) (app (Servant.Handler . ExceptT . try . runInIO))
