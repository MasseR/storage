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

newtype API route
  = API { index :: route :- Get '[JSON] Text }
  deriving stock (Generic)

type Reqs r m =
  ( MonadReader r m
  , MonadIO m
  )

handler :: Reqs r m => API (AsServerT m)
handler = API { index = pure "hello" }
