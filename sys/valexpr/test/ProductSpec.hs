{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ProductSpec where

import           Control.Arrow   (second)
import qualified Data.List       as List
import           Test.Hspec
import           Test.QuickCheck

import           FreeMonoidX     (fromDistinctAscPowerListT, fromListT,
                                  toDistinctAscOccurListT)
import           Product
import           ProductGen

-- * Properties
prop_ProductPower :: ProductGen Integer -> Integer -> Bool
prop_ProductPower (ProductGen s) 0 =
  power 0 s == fromListT []
prop_ProductPower (ProductGen s) n =
  power n s == ( fromDistinctAscPowerListT
                 . List.map (second (n *))
                 . toDistinctAscOccurListT
               ) s

prop_ProductEq :: ProductGen Integer -> Bool
prop_ProductEq (ProductGen s) =
    not (s /= s)

prop_ProductOrd :: ProductGen Integer -> Bool
prop_ProductOrd (ProductGen s) =
    s >= s

prop_ProductShow :: ProductGen Integer -> Bool
prop_ProductShow (ProductGen s) =
    show [s] == show [s]

spec :: Spec
spec =
  describe "Properties: " $ do
    it "correctly implements exponentiation" $ property prop_ProductPower
    it "correctly implements inequality" $ property prop_ProductEq
    it "correctly implements the order relation" $ property prop_ProductOrd
    it "correctly implements show" $ property prop_ProductShow