{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Serialize.Instances where

import           Data.List.NonEmpty (NonEmpty (..), fromList, toList)

import           Data.Serialize     (Serialize (..))

import           MyPrelude

instance Serialize a => Serialize (NonEmpty a) where
  put = put . toList
  get = fromList <$> get
