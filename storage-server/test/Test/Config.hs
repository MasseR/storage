{-# LANGUAGE TypeApplications #-}
module Test.Config where

import           Test.Golden
import           Test.Hspec
import           Test.Validity.Aeson

import           Data.Config

spec :: Spec
spec = describe "Config" $ do
  jsonSpecOnValid @Config
  golden @Config
