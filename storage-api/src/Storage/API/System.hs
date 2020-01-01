module Storage.API.System (API(..)) where

import           Servant.API
import           Servant.API.Generic

import qualified Storage.API.System.Stats as Stats

newtype API route
  = API { stats :: route :- "stats" :> ToServant Stats.API AsApi }
  deriving Generic
