{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Golden (golden) where

-- XXX: Consider moving this to a central place

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck         (Arbitrary)

import           Data.Aeson              (FromJSON, ToJSON)

settings :: Settings
settings = defaultSettings { useModuleNameAsSubDirectory = True }

golden :: forall a. (Typeable a, Arbitrary a, ToJSON a, FromJSON a) => Spec
golden = goldenSpecs @a settings Proxy
