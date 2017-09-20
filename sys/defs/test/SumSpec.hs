{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module SumSpec where

import           Control.Arrow   (second)
import qualified Data.List       as List
import           Test.Hspec
import           Test.QuickCheck

import           GenSum
import           Sum

-- * Properties
prop_SumMultiply :: GenSum Integer -> Integer -> Bool
prop_SumMultiply (GenSum s) 0 =
  multiply 0 s == Sum.fromList []
prop_SumMultiply (GenSum s) n =
  multiply n s == ( fromDistinctAscMultiplierList
                   . List.map (second (n *))
                   . toDistinctAscMultiplierList
                  ) s

prop_SumEq :: GenSum Integer -> Bool
prop_SumEq (GenSum s) =
    not (s /= s)

prop_SumOrd :: GenSum Integer -> Bool
prop_SumOrd (GenSum s) =
    s >= s

prop_SumShow :: GenSum Integer -> Bool
prop_SumShow (GenSum s) =
    show [s] == show [s]

spec :: Spec
spec =
  describe "Properties: " $ do
    it "correctly implements multiplication" $ property prop_SumMultiply
    it "correctly implements inequality" $ property prop_SumEq
    it "correctly implements the order relation" $ property prop_SumOrd
    it "correctly implements show" $ property prop_SumShow

