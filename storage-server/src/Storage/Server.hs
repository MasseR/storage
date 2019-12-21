{-# LANGUAGE ConstraintKinds  #-}
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

api :: Proxy (ToServantApi API.API)
api = genericApi @API.API Proxy

type Constraint r m = (API.Reqs r m, MonadReader r m, HasPort r, MonadIO m)

app :: forall r m. Constraint r m => (forall x. m x -> Servant.Handler x) -> Application
app nat =
  serve api (hoistServer api nat (genericServerT API.handler))

server :: (MonadUnliftIO m, Constraint r m) => m ()
server = do
  Port p <- view port
  withRunInIO $ \runInIO ->
    run (fromIntegral p) (app (Servant.Handler . ExceptT . try . runInIO))
