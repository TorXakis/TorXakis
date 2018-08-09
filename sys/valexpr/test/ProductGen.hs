{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ProductGen
( ProductGen(..)
)
where

import           Test.QuickCheck

import           FreeMonoidX         (fromOccurListT)
import           Product

newtype ProductGen a = ProductGen (FreeProduct a)
    deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (ProductGen a) where
  arbitrary = do
    args   <- listOf arbitrary
    powers <- vectorOf (length args) arbitrary
    return $ ProductGen (fromOccurListT (zip args powers))