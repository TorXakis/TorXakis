{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module ProductSpec where

import           Control.Arrow   (second)
import qualified Data.List       as List
import           Test.Hspec
import           Test.QuickCheck

import           GenProduct
import           Product


-- * Properties
prop_ProductPower :: GenProduct Integer -> Integer -> Bool
prop_ProductPower (GenProduct s) 0 =
  power 0 s == Product.fromList []
prop_ProductPower (GenProduct s) n =
  power n s == ( fromDistinctAscPowerList
                 . List.map (second (n *))
                 . toDistinctAscPowerList
               ) s

prop_ProductEq :: GenProduct Integer -> Bool
prop_ProductEq (GenProduct s) =
    not (s /= s)

prop_ProductOrd :: GenProduct Integer -> Bool
prop_ProductOrd (GenProduct s) =
    s >= s

prop_ProductShow :: GenProduct Integer -> Bool
prop_ProductShow (GenProduct s) =
    show [s] == show [s]

spec :: Spec
spec =
  describe "Properties: " $ do
    it "correctly implements exponentiation" $ property prop_ProductPower
    it "correctly implements inequality" $ property prop_ProductEq
    it "correctly implements the order relation" $ property prop_ProductOrd
    it "correctly implements show" $ property prop_ProductShow
