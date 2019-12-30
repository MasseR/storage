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
import           Servant.Server.Generic

import           MyPrelude

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  )

newtype API route
  = API { post :: route :- Post '[JSON] Text }
  deriving stock (Generic)


handler :: Reqs r m => API (AsServerT m)
handler =
  API { post = pure "foo" }
