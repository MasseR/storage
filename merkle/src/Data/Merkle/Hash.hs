{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Merkle.Hash where

import           MyPrelude

import           Crypto.Hash                 (Digest, SHA256)
import qualified Crypto.Hash                 as H
import           Data.Binary                 (Binary (..))
import           Data.ByteArray              (convert)
import qualified Data.ByteString.Base16      as B16

import           Control.Lens                (view, Prism', prism', re, preview)
import           Data.Text.Strict.Lens       (utf8)

import           Data.List.NonEmpty          (NonEmpty)

import           Data.Aeson                  (ToJSON (..), FromJSON (..))

import Web.HttpApiData (FromHttpApiData(..))

-- For testing
import           Data.GenValidity
import           Data.GenValidity.ByteString ()

newtype Hash = Hash { getHash :: Digest SHA256 }
  deriving (Show, Eq, Generic)

newtype B16 = B16 { get16 :: Text } deriving (Show, Eq, Generic, ToJSON, FromJSON, Binary)

hashed :: Prism' B16 Hash
hashed = prism' toB16 fromB16
  where
    toB16 = B16 . view utf8 . B16.encode . convert . getHash
    fromB16 = fmap Hash . H.digestFromByteString @_ @ByteString . view (re utf8) . get16

_Text :: Prism' Text B16
_Text = prism' get16 (Just . B16) -- Should this have some verification steps?

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

instance Binary Hash where
  put = put . view (re hashed)
  get = maybe (fail "Not a digest") pure . preview hashed =<< get

instance ToJSON Hash where
  toJSON = toJSON . view (re hashed)

instance FromJSON Hash where
  parseJSON o = maybe (fail "Not a digest") pure . preview hashed =<< parseJSON o

instance FromHttpApiData Hash where
  parseUrlPiece = note "Not a digest" . preview hashed . B16

instance Validity Hash where
  validate = trivialValidation

instance GenValid Hash where
  genValid = hash @ByteString <$> genValid
  shrinkValid _ = []
