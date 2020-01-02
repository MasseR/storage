{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Storage.API
  ( API(..) )
  where

import           Servant.API
import           Servant.API.Generic

import           MyPrelude

import qualified Storage.API.Object  as Object
import qualified Storage.API.System  as System


data API route
  = API { index  :: route :- Get '[JSON] Text
        , object :: route :- "object" :> ToServant Object.API AsApi
        , system :: route :- "system" :> ToServant System.API AsApi
        }
  deriving stock (Generic)

