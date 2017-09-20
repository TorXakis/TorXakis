{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module SumSpec where

import           Control.Arrow   (second)
import qualified Data.List       as List
import           Prelude         hiding (subtract, sum)
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

-- * Test data
sum6 :: Sum Int
sum6 = fromList [1, 2, 3]

sum10 :: Sum Int
sum10 = fromList [2, 2, 6]

-- * Compute the value of the sum.
theSumOf :: Sum Int -> Int
theSumOf = foldMultiplier (\x _ -> (x +)) 0

spec :: Spec
spec = do
  describe "Properties: " $ do
    it "correctly implements multiplication" $ property prop_SumMultiply
    it "correctly implements inequality" $ property prop_SumEq
    it "correctly implements the order relation" $ property prop_SumOrd
    it "correctly implements show" $ property prop_SumShow
  describe "Examples: " $ do
    it "sum6 is should be 6" $
      theSumOf sum6 `shouldBe` 6
    it "sum10 is should be 10" $
      theSumOf sum6 `shouldBe` 6
    it "sum6 + sum10 should be 16" $
      theSumOf (sum6 `sum` sum10) `shouldBe` 16
    it "sum6 - 10 should be -4" $
      theSumOf (subtract 10 sum6) `shouldBe` -4
    it "sum6 * 0 should be 0" $
      theSumOf (multiply 0 sum6) `shouldBe` 0
    it "sum6 * 2 should be 12" $
      theSumOf (multiply 2 sum6) `shouldBe` 12

