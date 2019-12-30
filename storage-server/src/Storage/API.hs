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

import qualified Storage.API.Object     as Object


data API route
  = API { index  :: route :- Get '[JSON] Text
        , object :: route :- "object" :> ToServant Object.API AsApi
        }
  deriving stock (Generic)

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  , Object.Reqs r m
  )

handler :: Reqs r m => API (AsServerT m)
handler =
  API { index = pure "hello"
      , object = toServant Object.handler
      }
