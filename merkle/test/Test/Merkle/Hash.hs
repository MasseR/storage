{-# LANGUAGE TypeApplications #-}
module Test.Merkle.Hash where


import           Test.Hspec

import           Data.Merkle.Hash
import           Data.SafeCopy       (safeGet, safePut)
import           Data.Serialize      (runGet, runPut)

import           Test.Validity
import           Test.Validity.Aeson

import           MyPrelude

spec :: Spec
spec = do
  describe "Serializing hashes to json" $
    jsonSpecOnValid @Hash
  describe "Binary serialization" $
    it "decode . encode == id" $
      inverseFunctionsIfSecondSucceedsOnValid @Hash (runPut . safePut) (runGet safeGet)
