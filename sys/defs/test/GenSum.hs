{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module GenSum
( GenSum(..)
)
where

import Test.QuickCheck
import Test.QuickCheck.Gen(chooseAny)

import Sum

newtype GenSum a = GenSum (Sum a)
    deriving (Show)
    
instance (Ord a, Arbitrary a) => Arbitrary (GenSum a) where
  arbitrary = do
    i <- choose (0,100)
    args <- vectorOf i arbitrary
    multipliers <- vectorOf i chooseAny
    return $ GenSum (Sum.fromMultiplierList (zip args multipliers))