module Storage.Client where

import Storage.API

-- import Servant.Client.Streaming
import Servant.Client.Generic
import Servant.Client.Core

import MyPrelude

client :: (RunStreamingClient m, Monad m) => API (AsClientT m)
client = genericClient
