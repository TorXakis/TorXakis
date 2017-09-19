{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module GenProduct
( GenProduct(..)
)
where

import Test.QuickCheck
import Test.QuickCheck.Gen(chooseAny)

import Product

newtype GenProduct a = GenProduct (Product a)
    deriving (Show)
    
instance (Ord a, Arbitrary a) => Arbitrary (GenProduct a) where
  arbitrary = do
    i <- choose (0,100)
    args <- vectorOf i arbitrary
    powers <- vectorOf i chooseAny
    return $ GenProduct (Product.fromPowerList (zip args powers))