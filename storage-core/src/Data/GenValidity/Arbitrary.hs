{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Data.GenValidity.Arbitrary
Description : Create arbitrary instances via genvalid
Copyright   : (c) Mats Rauhala, 2020
License     : MIT
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

Create 'Arbitrary' instances via 'GenValid'

@
data Person
  = Person { name :: Text
           , age :: Int
           }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Validity)
  deriving (Arbitrary) via (ViaGenValid Person)

instance GenValid Person where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
@
-}
module Data.GenValidity.Arbitrary
  ( Arbitrary
  , ViaGenValid(..)
  )
  where

import           Data.GenValidity
import           Test.QuickCheck  (Arbitrary (..))

-- | A 'DerivingVia' style newtype for generating 'Arbitrary' instances
newtype ViaGenValid a = ViaGenValid a
  deriving newtype (GenValid, Validity)

instance GenValid a => Arbitrary (ViaGenValid a) where
  arbitrary = genValid
  shrink = shrinkValid
