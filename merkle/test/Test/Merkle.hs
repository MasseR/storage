module Test.Merkle where

import           Test.Hspec

import Data.Merkle

import           Test.Validity

spec :: Spec
spec =
  describe "Merging trees" $ do
    it "Merging two valid trees creates a valid tree" $
      producesValidsOnValids2 merge
    it "Merging a list of trees creates a list of valid trees" $
      producesValidsOnValids mergeLayer
    it "Merging a list of trees consumes trees" $
      forAllValid $ \trees -> length trees >= length (mergeLayer trees)
    it "Combining a list of trees creates a valid tree" $
      producesValidsOnValids combine
