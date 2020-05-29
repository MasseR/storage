{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Storage.API.Object
  ( API(..) )
  where

import           Servant.API
import           Servant.API.Generic
import           Servant.Pipes       ()

import           MyPrelude

import           Pipes

import           Data.Merkle.Hash

data API route
  = API { postObject :: route :- StreamBody NoFraming OctetStream (Producer ByteString IO ()) :> Post '[JSON] Hash
        , getObject :: route :- Capture "hash" Hash :> StreamGet NoFraming OctetStream (Producer ByteString IO ())
        , getObjects :: route :- Get '[JSON] [Hash]
        }
  deriving stock (Generic)


