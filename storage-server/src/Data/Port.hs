{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Port where

import           Control.Lens (Lens', lens)
import           Data.Aeson   (FromJSON, ToJSON)
import           MyPrelude

newtype Port = Port Natural deriving (Show)

deriving newtype instance ToJSON Port
deriving newtype instance FromJSON Port

class HasPort a where
  getPort :: a -> Port
  setPort :: a -> Port -> a

port :: HasPort a => Lens' a Port
port = lens getPort setPort
