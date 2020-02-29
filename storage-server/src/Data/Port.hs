{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Data.Port where

import           Control.Lens (Lens', lens)
import           Data.Aeson   (FromJSON, ToJSON)
import           MyPrelude

import Test.QuickCheck (Positive(..))
import Data.GenValidity

newtype Port = Port Natural deriving (Show, Eq)

deriving newtype instance ToJSON Port
deriving newtype instance FromJSON Port
deriving newtype instance Validity Port
deriving newtype instance GenValid Port

class HasPort a where
  getPort :: a -> Port
  setPort :: a -> Port -> a

port :: HasPort a => Lens' a Port
port = lens getPort setPort
