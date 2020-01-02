module Storage.API.System.Stats
  ( API(..)
  , Counter(..)
  , Gauge(..)
  ) where


import           Servant.API
import           Servant.API.Generic

import           Data.Aeson          (FromJSON, ToJSON)

import           MyPrelude

newtype Counter = Counter Int64 deriving newtype (ToJSON, FromJSON)

newtype Gauge = Gauge Int64 deriving newtype (ToJSON, FromJSON)

data API route
  = API { requests200 :: route :- "requests" :> "200" :> Get '[JSON] Counter
        , requests500 :: route :- "requests" :> "500" :> Get '[JSON] Counter
        , requests :: route :- "requests" :> Get '[JSON] Counter
        , gcBytesAllocated :: route :- "gc" :> "bytes" :> "allocated" :> Get '[JSON] Counter
        , gcMaxBytesUsed :: route :- "gc" :> "bytes" :> "max" :> Get '[JSON] Gauge
        , gcCurrentBytesUsed :: route :- "gc" :> "bytes" :> "current" :> Get '[JSON] Gauge
        }
  deriving Generic
