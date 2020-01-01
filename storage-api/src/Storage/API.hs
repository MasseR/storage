{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Storage.API
  ( API(..) )
  where

import           Servant.API
import           Servant.API.Generic

import           MyPrelude

import qualified Storage.API.Object     as Object


data API route
  = API { index  :: route :- Get '[JSON] Text
        , object :: route :- "object" :> ToServant Object.API AsApi
        }
  deriving stock (Generic)

