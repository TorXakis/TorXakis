{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RegexGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'RegexGen'.
-----------------------------------------------------------------------------
module TorXakis.RegexGenSpec
(spec
)
where
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Cover
import           TorXakis.Error
import           TorXakis.Regex
import           TorXakis.RegexGen

-- | Eq check
prop_Eq :: RegexGen -> Bool 
prop_Eq (RegexGen a) = coverEq a

-- | Ord check
prop_Ord :: RegexGen -> Bool 
prop_Ord (RegexGen a) = coverOrd a

-- | Read Show check
prop_ReadShow :: RegexGen -> Bool
prop_ReadShow (RegexGen val) = coverReadShow val

-- | FromXsd . ToXsd == Identity
prop_ToFromXsd :: RegexGen -> Bool
prop_ToFromXsd (RegexGen val) =
    let xsd = toXsd val in
        case fromXsd xsd of
            Left e     -> trace ("\nParse error " ++ show e ++ " on\n" ++ show xsd) False
            Right val' -> if val' == val then True
                                         else trace ("\nval\n" ++ show val ++ "\nxsd\n" ++ show xsd ++ "\nval'\n" ++ show val' ++ "\nxsd'\n" ++ show (toXsd val')) False

-- | Concat (xs ++ [RegexEmpty] ++ ys) == Concat (xs ++ ys)
prop_ConcatEmpty :: [RegexGen] -> Gen Bool
prop_ConcatEmpty rgs =
    let rs = map unRegexGen rgs in do
        p <- choose (0, length rs)
        let (xs,ys) = splitAt p rs
            actual = mkRegexConcat (xs++(mkRegexEmpty : ys))
            expected = mkRegexConcat (xs++ys) in
                if actual == expected
                then return True
                else trace ("\nactual xsd\n" ++ show (toXsd actual) ++ "\nexpected xsd\n" ++ show (toXsd expected)) $ return False

-- | Union xs == Union (reverse xs)
prop_UnionOrder :: [RegexGen] -> Bool
prop_UnionOrder xgs =
    let xs = map unRegexGen xgs
        actual = mkRegexUnion (reverse xs)
        expected = mkRegexUnion xs in
            actual == expected

-- | Loop r 0 (just 0) == RegexEmpty
prop_LoopExactlyZero :: RegexGen -> Bool
prop_LoopExactlyZero (RegexGen val) =
    let actual = mkRegexLoop val 0 (Just 0)
        expected :: Either Error Regex
        expected = Right mkRegexEmpty in
            actual == expected

-- | Loop r 1 (just 1) == r
prop_LoopExactlyOnce :: RegexGen -> Bool
prop_LoopExactlyOnce (RegexGen val) =
    let actual = mkRegexLoop val 1 (Just 1)
        expected :: Either Error Regex
        expected = Right val in
            actual == expected

spec :: Spec
spec = do
  describe "A Generated Regex" $ do
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
    it "fromXsd . toXsd is identity" $ property prop_ToFromXsd
  describe "A Regex can be rewritten:" $ do
    it "Empty is irrelevant in concat" $ property prop_ConcatEmpty
    it "Order is irrelevant in union" $ property prop_UnionOrder
    it "Loop exactly zero is Empty" $ property prop_LoopExactlyZero
    it "Loop exactly once is identity" $ property prop_LoopExactlyOnce
