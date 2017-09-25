{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module GenProduct
( GenProduct(..)
)
where

import           Test.QuickCheck
import           Test.QuickCheck.Gen (chooseAny)

import           FreeMonoidX         (fromMultiplierListT)
import           Product

newtype GenProduct a = GenProduct (FreeProduct a)
    deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (GenProduct a) where
  arbitrary = do
    args   <- listOf arbitrary
    powers <- vectorOf (length args) arbitrary
    return $ GenProduct (fromMultiplierListT (zip args powers))
