{-# LANGUAGE TypeApplications #-}
module Test.Config.Metrics where

import Test.Golden
import Test.Hspec
import Test.Validity.Aeson

-- System under test
import Data.Config.Metrics

spec :: Spec
spec = describe "Data.Config.Metrics" $ do
  jsonSpecOnValid @Metrics
  golden @Metrics
