module Storage.Environment
  ( Env(..) )
  where

import           Control.Lens          (set, view)
import           Data.Generics.Product (typed)
import           MyPrelude
import           Storage.Logger        (HasLoggingEnv (..), LoggingEnv)

newtype Env = Env { loggingEnv :: LoggingEnv }
  deriving Generic

instance HasLoggingEnv Env where
  getLoggingEnv = view typed
  setLoggingEnv = flip (set typed)
