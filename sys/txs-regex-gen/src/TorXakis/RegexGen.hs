{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.RegexGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.Regex'.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.RegexGen
( 
-- * Regex Generator
  RegexGen(..)
  -- dependencies, yet part of interface
, Regex
)
where


import           Control.DeepSeq (NFData)
import           Data.Char
import           Data.Data (Data)
import qualified Data.Text as T
import           GHC.Generics     (Generic)
import           Test.QuickCheck

import           TorXakis.Distribute
import           TorXakis.Regex

-- | Definition of the name generator.
newtype RegexGen = RegexGen { -- | accessor to 'TorXakis.Regex'
                            unRegexGen :: Regex}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | generate a char within the allowed range
arbitraryChar :: Gen Char
arbitraryChar = chr <$> choose (0, 255)

-- ----------------------------------------------------------------------------------
genRegexStringLiteral :: Gen RegexGen
genRegexStringLiteral = do
    s <- listOf arbitraryChar
    return $ RegexGen (mkRegexStringLiteral (T.pack s))

genRegexRange :: Gen RegexGen
genRegexRange = do
    c1 <- arbitraryChar
    c2 <- arbitraryChar
    let (l,u) = if c1 <= c2 then (c1,c2) else (c2,c1) in
        case mkRegexRange l u of
            Left e -> error ("mkRegexRange failed with lowerbound (" ++ show l ++ ") and upperbound (" ++ show u ++ ") with error " ++ show e)
            Right r -> return $ RegexGen r

-- | prevent loops with large values for the upper and lowerbound of occurances to ensure fast regression tests
regexLoopPenalty :: Int
regexLoopPenalty = 10

genRegexLoop :: Gen RegexGen
genRegexLoop = do
    size <- getSize
    b <- arbitrary :: Gen Bool
    if b
    then do
        n1 <- choose(0, size `div` regexLoopPenalty)
        rg <- genRegex (size `div` (n1+1))
        let r = unRegexGen rg in
            case mkRegexLoop r (toInteger n1) Nothing of
                Left e -> error ("mkRegexLoop failed with lowerbound (" ++ show n1 ++ ") and no upperbound with error " ++ show e)
                Right l -> return $ RegexGen l
    else do
        n1 <- choose(0, size `div` regexLoopPenalty)
        n2 <- choose(0, size `div` regexLoopPenalty)
        let (l,u) = if n1 <= n2 then (n1,n2) else (n2,n1) in do
            rg <- genRegex (size `div` (l+1))
            let r = unRegexGen rg in
                case mkRegexLoop r (toInteger l) (Just (toInteger u)) of
                    Left e -> error ("mkRegexLoop failed with lowerbound (" ++ show l ++ ") and upperbound (" ++ show u ++ ") with error " ++ show e)
                    Right l' -> return $ RegexGen l'
    
genRegexConcat :: Gen RegexGen
genRegexConcat = do
    size <- getSize
    rs <- serie 0 size
    return $ RegexGen (mkRegexConcat (map unRegexGen rs))

genRegexUnion :: Gen RegexGen
genRegexUnion = do
    size <- getSize
    rs <- serie 1 size
    case mkRegexUnion (map unRegexGen rs) of
        Left e -> error ("mkRegexUnion failed on non-empty list with error " ++ show e)
        Right r -> return $ RegexGen r

-- -----------------------------------------------------------------------------------
serieSize :: Int
serieSize = 10

serie :: Int -> Int -> Gen [RegexGen]
serie lb size | size < lb + serieSize = error ("illegal call: size (" ++ show size ++ ") must be larger than or equal to lowerbound (" ++ show lb ++ ") plus serieSize (" ++ show serieSize ++ ").")
serie lb size             =
    let available = size `div` serieSize in do
        rnd <- choose (0, available)
        let nrofElems = lb + rnd
            remaining = available - rnd in do
            additionalComplexity <- distribute remaining nrofElems
            mapM genRegex additionalComplexity

allRegexGenThresholdTuples :: [(Gen RegexGen, Int)]
allRegexGenThresholdTuples = [ (genRegexStringLiteral,   0)
                             , (genRegexRange,           2)
                             , (genRegexLoop,            3)
                             , (genRegexConcat,          serieSize +0)
                             , (genRegexUnion,           serieSize +1)
                             ]

selectRegexGen :: Int -> [Gen RegexGen]
selectRegexGen t = map fst (filter (\(_,a) -> a <= t) allRegexGenThresholdTuples)
                            
genRegex :: Int -> Gen RegexGen
genRegex t = oneof (selectRegexGen t)

instance Arbitrary RegexGen
    where
        arbitrary = sized genRegex