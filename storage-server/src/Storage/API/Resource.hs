{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Storage.API.Resource
  ( API
  , handler
  , Reqs
  )
  where

import           Servant.API
import           Servant.API.Generic
import           Servant.Pipes          ()
import           Servant.Server.Generic

import           MyPrelude

import           Pipes
-- import qualified Pipes.Prelude          as P

import           Control.Lens           (view)
import           Data.Text.Strict.Lens  (utf8)
import           Storage.Logger

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , MonadUnliftIO m
  , WithLogging m
  )

newtype API route
  = API { post :: route :- StreamBody NoFraming OctetStream (Producer ByteString IO ()) :> PostNoContent '[JSON] () }
  deriving stock (Generic)


handler :: Reqs r m => API (AsServerT m)
handler =
  API { post = \x -> withRunInIO $ \r -> runEffect (for x (liftIO . r . logInfo . view utf8)) }
