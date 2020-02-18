{-# LANGUAGE TypeApplications #-}
module Test.Storage.API.System.Stats where

import           Test.Hspec

import           Test.Validity.Aeson

import           Storage.API.System.Stats

spec :: Spec
spec = do
  jsonSpecOnValid @Counter
  jsonSpecOnValid @Gauge
