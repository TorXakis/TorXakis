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
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Cover
import           TorXakis.Regex
import           TorXakis.Regex.Xsd
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
prop_ToFromXsd :: RegexGen -> Expectation
prop_ToFromXsd (RegexGen val) =
    let xsd = toXsd val in
        case fromXsd xsd of
            Left e     -> error ("Parse error " ++ show e ++ " on\n" ++ show xsd)
            Right val' -> val' `shouldBe` val

-- | Empty Regex in loop is just empty regex
-- forall l    : "(){l,}" == "()"
-- forall l, u : "(){l,u}" == "()"
prop_Equivalent_EmptyInLoop :: LoopBound -> Expectation
prop_Equivalent_EmptyInLoop (LoopBound l mu) =
    case mkRegexLoop mkRegexEmpty l mu of
        Left e     -> error ("mkRegexLoop unexpectedly failed with " ++ show e)
        Right loop -> loop `shouldBe` mkRegexEmpty

-- | Loop with zero occurances is empty regex
-- forall r : "r{0,0}" == "()"
prop_Equivalent_LoopZeroIsEmpty :: RegexGen -> Expectation
prop_Equivalent_LoopZeroIsEmpty (RegexGen r) = 
    case mkRegexLoop r 0 (Just 0) of
        Left e     -> error ("mkRegexLoop unexpectedly failed with " ++ show e)
        Right loop -> loop `shouldBe` mkRegexEmpty

-- | Loop with one occurance is just that regex
-- forall r : "r{1,1}" == "r"
prop_Equivalent_LoopOnceIsIdentity :: RegexGen -> Expectation
prop_Equivalent_LoopOnceIsIdentity (RegexGen r) = 
    case mkRegexLoop r 1 (Just 1) of
        Right loop -> loop `shouldBe` r
        Left e     -> error ("mkRegexLoop unexpectedly failed with " ++ show e)

-- | Empty in concat is removable
-- | Concat (xs ++ [RegexEmpty] ++ ys) == Concat (xs ++ ys)
prop_Equivalent_EmptyInConcatRemovable :: [RegexGen] -> Expectation
prop_Equivalent_EmptyInConcatRemovable rgs = 
        checkAll `shouldSatisfy` and
    where
        rs :: [Regex]
        rs = map unRegexGen rgs
        
        checkAll :: [Bool]
        checkAll = map check [0..length rs]
        
        check :: Int -> Bool
        check p = let (xs,ys) = splitAt p rs
                      actual = mkRegexConcat (xs++(mkRegexEmpty : ys))
                      expected = mkRegexConcat (xs++ys) in
                          actual == expected

-- | A singleton range is allowed and equal to that char
prop_Equivalent_SingletonRange :: RegexChar -> Expectation
prop_Equivalent_SingletonRange (RegexChar c) = 
    case mkRegexRange c c of
        Left e      -> error ("mkRegexRange unexpectedly failed with " ++ show e)
        Right range -> case mkRegexCharLiteral c of
                        Left e      -> error ("mkRegexCharLiteral unexpectedly failed with " ++ show e)
                        Right expected -> range `shouldBe` expected

-- | nested loops can be flattened
-- when innerloop has different bounds
prop_Equivalent_LoopNested :: LoopBound -> LoopBound -> RegexGen -> Expectation
prop_Equivalent_LoopNested (LoopBound li mui)            _                             _            | Just li == mui    =
    return ()  -- no rewrite possible: (a{5,5}){1,2} <> a{5,10}
prop_Equivalent_LoopNested lbi@(LoopBound li (Just ui))  lbo@(LoopBound lo (Just uo))  (RegexGen r)                     =
    let actual = nestedLoop lbi lbo r in 
        case mkRegexLoop r (li*lo) (Just (ui*uo)) of
            Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
            Right expected -> actual `shouldBe` expected
prop_Equivalent_LoopNested lbi@(LoopBound li _)          lbo@(LoopBound lo _)          (RegexGen r)                     =
    let actual = nestedLoop lbi lbo r in 
        case mkRegexLoop r (li*lo) Nothing of
            Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
            Right expected -> actual `shouldBe` expected

-- | create a nested loop
nestedLoop :: LoopBound -> LoopBound -> Regex -> Regex
nestedLoop (LoopBound li mui) (LoopBound lo muo) r =
    case mkRegexLoop r li mui of
        Left e      -> error ("mkRegexLoop innerloop unexpectedly failed with " ++ show e)
        Right loopi -> case mkRegexLoop loopi lo muo of
                        Left e      -> error ("mkRegexLoop outerloop unexpectedly failed with " ++ show e)
                        Right loopo -> loopo

-- | concatenated loops over the same regular expression can be combined
prop_Equivalent_LoopConcatenated :: LoopBound -> LoopBound -> RegexGen -> Expectation
prop_Equivalent_LoopConcatenated lb1@(LoopBound l1 (Just u1)) lb2@(LoopBound l2 (Just u2)) (RegexGen r) =
    case mkRegexLoop r (l1+l2) (Just (u1+u2)) of
        Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
        Right expected -> let actual = concatLoop lb1 lb2 r in
                                actual `shouldBe` expected
prop_Equivalent_LoopConcatenated lb1@(LoopBound l1 _)         lb2@(LoopBound l2 _)         (RegexGen r) =
    case mkRegexLoop r (l1+l2) Nothing of
        Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
        Right expected -> let actual = concatLoop lb1 lb2 r in
                                actual `shouldBe` expected

concatLoop :: LoopBound -> LoopBound -> Regex -> Regex
concatLoop (LoopBound l1 m1) (LoopBound l2 m2) r =
    case mkRegexLoop r l1 m1 of
        Left e      -> error ("mkRegexLoop first loop unexpectedly failed with " ++ show e)
        Right loop1 -> case mkRegexLoop r l2 m2 of
                        Left e      -> error ("mkRegexLoop second loop unexpectedly failed with " ++ show e)
                        Right loop2 -> mkRegexConcat [loop1, loop2]

-- | loop over an instance followed by that instance can be combined
-- rewritting "a{3,6}a" to "a{4,7}" doesn't always simplify the str.in.re problem.
-- Yet in some contexts it does! For example, rewritting "a{3,6}aa{1,2}" to "a{5,9}" simplifies the str.in.re problem.
-- Note when a concatenated regex is in a loop, we must look at the front/tail of any concatenated regex after/before it.
prop_Equivalent_LoopAndInstance :: LoopBound -> RegexGen -> Expectation
prop_Equivalent_LoopAndInstance (LoopBound l m) (RegexGen r) =
    let lExpected = l +1
        mExpected = case m of
                        Nothing -> Nothing
                        Just u  -> Just (u+1)
      in
        case mkRegexLoop r lExpected mExpected of
            Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
            Right expected -> case mkRegexLoop r l m of
                                    Left e     -> error ("mkRegexLoop loop unexpectedly failed with " ++ show e)
                                    Right loop -> mkRegexConcat [loop, r] `shouldBe` expected

-- | an instance followed by a loop over that instance can be combined
prop_Equivalent_InstanceAndLoop :: LoopBound -> RegexGen -> Expectation
prop_Equivalent_InstanceAndLoop (LoopBound l m) (RegexGen r) =
    let lExpected = l +1
        mExpected = case m of
                        Nothing -> Nothing
                        Just u  -> Just (u+1)
      in
        case mkRegexLoop r lExpected mExpected of
            Left e         -> error ("mkRegexLoop (expected) unexpectedly failed with " ++ show e)
            Right expected -> case mkRegexLoop r l m of
                                    Left e     -> error ("mkRegexLoop loop unexpectedly failed with " ++ show e)
                                    Right loop -> mkRegexConcat [r,loop] `shouldBe` expected

-- | Union xs == Union (reverse xs)
prop_UnionOrder :: [RegexGen] -> Expectation
prop_UnionOrder xgs =
    let xs = map unRegexGen xgs
        actual = mkRegexUnion (reverse xs)
        expected = mkRegexUnion xs in
            actual `shouldBe` expected


spec :: Spec
spec = do
  describe "A Generated Regex" $ do
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
    it "fromXsd . toXsd is identity" $ property prop_ToFromXsd
  describe "A Regex can be rewritten:" $ do
        it "Empty regex in loop is empty regex" $ property prop_Equivalent_EmptyInLoop
        it "Loop exactly zero is Empty" $ property prop_Equivalent_LoopZeroIsEmpty
        it "Loop exactly once is identity" $ property prop_Equivalent_LoopOnceIsIdentity
        it "Empty is irrelevant in concat" $ property prop_Equivalent_EmptyInConcatRemovable
        it "singleton range is character" $ property prop_Equivalent_SingletonRange
        it "nested loops" $ property prop_Equivalent_LoopNested
--        it "concatenate loops over same regex" $ property prop_Equivalent_LoopConcatenated
        it "combine loop over instance followed by that instance" $ property prop_Equivalent_LoopAndInstance
--        it "combine instance followed by a loop over that instance" $ property prop_Equivalent_InstanceAndLoop
        it "Order is irrelevant in union" $ property prop_UnionOrder
