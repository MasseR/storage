{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Storage.API
  ( API
  , Reqs
  , handler
  )
  where

import           Servant.API
import           Servant.API.Generic
import           Servant.Server.Generic

import           MyPrelude

import qualified Storage.API.Resource   as Resource


data API route
  = API { index    :: route :- Get '[JSON] Text
        , resource :: route :- "resource" :> ToServant Resource.API AsApi
        }
  deriving stock (Generic)

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , Resource.Reqs r m
  )

handler :: Reqs r m => API (AsServerT m)
handler =
  API { index = pure "hello"
      , resource = toServant Resource.handler
      }
