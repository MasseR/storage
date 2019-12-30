{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Merkle
  ( module Data.Merkle
  , Hash
  , Hashable(..)
  ) where

import           MyPrelude

import           Data.Merkle.Hash

import           Data.List.NonEmpty          (NonEmpty (..), nonEmpty, unfoldr)

import           Control.Comonad

-- For testing
import           Data.GenValidity
import           Data.GenValidity.ByteString ()

data Merkle a
  = Node a (NonEmpty (Merkle a))
  | Leaf a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Comonad Merkle where
  extract = \case
    Node a _ -> a
    Leaf a -> a
  extend f = \case
    Leaf a -> Leaf (f $ Leaf a)
    Node a cs -> Node (f (Node a cs)) (fmap (extend f) cs)

merkleValue :: Merkle a -> a
merkleValue = \case
  Node a _ -> a
  Leaf a -> a

instance Validity (Merkle Hash) where
  validate = \case
    Leaf _ -> mempty
    Node h cs -> check (h == hash cs) "Parent hash is left + right"

instance GenValid (Merkle Hash) where
  genValid = Leaf <$> genValid
  shrinkValid = shrinkValidStructurally

instance Hashable a => Hashable (Merkle a) where
  hash = \case
    Leaf a -> hash a
    Node a _ -> hash a

merge :: Merkle Hash -> Merkle Hash -> Merkle Hash
merge l r = Node (hash (l,r)) (l :| [r])

mergeLayer :: NonEmpty ( Merkle Hash ) -> NonEmpty ( Merkle Hash )
mergeLayer = fmap (uncurry merge) . unfoldr chunks
  where
    chunks = \case
      a :| (b : xs) -> ( (a, b), nonEmpty xs)
      a :| [] -> ( (a, Leaf (hash @ByteString "")), Nothing)

combine :: NonEmpty ( Merkle Hash ) -> Merkle Hash
combine = \case
  x :| [] -> x
  xs -> combine (mergeLayer xs)
