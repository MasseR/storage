{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Storage.Environment
  ( Env(..) )
  where

import           Control.Lens          (set, view)
import           Data.Generics.Product (field, typed)
import           MyPrelude
import           Storage.Logger        (HasLoggingEnv (..), LoggingEnv)

import           Data.Config
import           Data.Port

data Env
  = Env { loggingEnv :: LoggingEnv
        , config     :: Config
        }
  deriving Generic

instance HasLoggingEnv Env where
  getLoggingEnv = view typed
  setLoggingEnv = flip (set typed)

instance HasPort Env where
  getPort = view (field @"config" . typed)
  setPort = flip (set (field @"config" . typed))
