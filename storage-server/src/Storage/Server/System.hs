{-# LANGUAGE ConstraintKinds #-}
module Storage.Server.System
  ( Reqs
  , handler
  )
  where

import           MyPrelude

import           Storage.API.System          (API (..))

import           Servant.API.Generic
import           Servant.Server.Generic

import qualified Storage.Server.System.Stats as Stats

type Reqs r m =
  ( MonadIO m
  , Stats.Reqs r m
  )

handler :: Reqs r m => API (AsServerT m)
handler = API { stats = toServant Stats.handler }

