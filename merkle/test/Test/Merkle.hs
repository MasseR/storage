{-# LANGUAGE TypeApplications #-}
module Test.Merkle where

import           Test.Hspec

import           Data.Merkle

import           Test.Validity

import           Control.Comonad

spec :: Spec
spec = do
  describe "Merging trees" $ do
    it "Merging two valid trees creates a valid tree" $
      producesValidsOnValids2 merge
    it "Merging a list of trees creates a list of valid trees" $
      producesValidsOnValids mergeLayer
    it "Merging a list of trees consumes trees" $
      forAllValid $ \trees -> length trees >= length (mergeLayer trees)
    it "Combining a list of trees creates a valid tree" $
      producesValidsOnValids combine
  describe "Comonad laws" $ do
    it "extend extract == id" $
      equivalentOnValid @(Merkle Hash) (extend extract) id
    it "extract . extend id == id" $
      equivalentOnValid @(Merkle Hash) (extract . extend hash) hash
    it "extend id . extend id == id . id" $ -- Not a good test
      equivalentOnValid @(Merkle Hash) (extend hash . extend hash) (extend (hash . extend hash))
    it "f =>= extract == f" $
      equivalentOnValid @(Merkle Hash) (hash =>= extract) hash
    it "extract =>= f == f" $
      equivalentOnValid @(Merkle Hash) (extract =>= hash) hash
    it "(f =>= g) =>= h == f =>= (g =>= h)" $
      equivalentOnValid @(Merkle Hash) ((hash =>= hash) =>= hash) (hash =>= (hash =>= hash))
    it "duplicate . duplicate == fmap duplicate . duplicate" $
      equivalentOnValid @(Merkle Hash) (duplicate . duplicate) (fmap duplicate . duplicate)
