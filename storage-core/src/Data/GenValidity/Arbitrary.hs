{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.GenValidity.Arbitrary
  ( Arbitrary
  , ViaGenValid(..)
  )
  where

import           Data.GenValidity
import           Test.QuickCheck  (Arbitrary (..))

newtype ViaGenValid a = ViaGenValid a
  deriving newtype (GenValid, Validity)

instance GenValid a => Arbitrary (ViaGenValid a) where
  arbitrary = genValid
  shrink = shrinkValid
