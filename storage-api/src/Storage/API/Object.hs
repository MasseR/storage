{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Storage.API.Object
  ( API(..) )
  where

import           Servant.API
import           Servant.API.Generic
import           Servant.Pipes          ()

import           MyPrelude

import           Pipes

import           Data.Merkle.Hash

data API route
  = API { post :: route :- StreamBody NoFraming OctetStream (Producer ByteString IO ()) :> Post '[JSON] Hash
        , get :: route :- Capture "hash" Hash :> StreamGet NoFraming OctetStream (Producer ByteString IO ()) }
  deriving stock (Generic)


