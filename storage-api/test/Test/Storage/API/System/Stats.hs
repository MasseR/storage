{-# LANGUAGE TypeApplications #-}
module Test.Storage.API.System.Stats where

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.Validity.Aeson

import           Storage.API.System.Stats

settings :: Settings
settings = defaultSettings { useModuleNameAsSubDirectory = True }

spec :: Spec
spec = do
  jsonSpecOnValid @Counter
  jsonSpecOnValid @Gauge
  goldenSpecs @Counter settings Proxy
  goldenSpecs @Gauge settings Proxy
