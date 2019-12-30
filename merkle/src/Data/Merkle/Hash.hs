{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Merkle.Hash where

import           MyPrelude

import           Crypto.Hash                 (Digest, SHA256)
import qualified Crypto.Hash                 as H
import           Data.ByteArray              (convert)
import qualified Data.ByteString.Base16      as B16

import           Control.Lens                (view)
import           Data.Text.Strict.Lens       (utf8)

import           Data.List.NonEmpty          (NonEmpty)

import           Data.Aeson                  (ToJSON (..))

-- For testing
import           Data.GenValidity
import           Data.GenValidity.ByteString ()


class Hashable a where
  hash :: a -> Hash

instance Hashable ByteString where
  hash = Hash . H.hash

instance Hashable Hash where
  hash = id


instance (Hashable a, Hashable b) => Hashable (a,b) where
  hash (a,b) = hash (a' <> b')
    where
      a' :: ByteString
      a' = convert (getHash (hash a))
      b' :: ByteString
      b' = convert (getHash (hash b))

instance Hashable a => Hashable (NonEmpty a) where
  hash = hash @ByteString . foldMap (convert . getHash . hash)

newtype Hash = Hash { getHash :: Digest SHA256 }
  deriving (Show, Eq, Generic)

instance ToJSON Hash where
  toJSON = toJSON . view utf8 . B16.encode . convert . getHash

instance Validity Hash where
  validate = trivialValidation

instance GenValid Hash where
  genValid = hash @ByteString <$> genValid
  shrinkValid _ = []
