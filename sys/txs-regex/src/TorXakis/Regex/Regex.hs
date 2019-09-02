{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.Regex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Regular Expressions.
-- We have chosen UTF-8 based on the needs of our current users
-- and the capabilities of the problem solvers.
-- UTF-8 has 256 characters from \x00 till \xFF
--
-- Note XSD and Posix Regex has two union operators (union operator and union of Char Group Parts)
-- Our implementation has like SMT Regex only one union operator.
-- Consequently, the Posix regular expression [A-Za-z_] will be printed as the equivalent expression [A-Z]|[a-z]|strLit '_'
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ViewPatterns          #-}
module TorXakis.Regex.Regex
( -- * Regular Expression
  RegexView (..)
, Regex
, view
, regexRangeLow
, regexRangeHigh
  -- * Constructors
, mkRegexEmpty
, mkRegexStringLiteral
, mkRegexConcat
, mkRegexUnion
, mkRegexLoop
, mkRegexOptional
, mkRegexKleeneStar
, mkRegexKleeneCross
, mkRegexRange
, mkRegexAll
, mkRegexDot
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Either
import           Data.Set
import           Data.Text
import           GHC.Generics        (Generic)

import           TorXakis.Error

-- | RegexView: the public view of regular expression 'Regex'
data RegexView =  RegexStringLiteral Text
                | RegexConcat [Regex]                     -- invariant: length list is at least 2, RegexEmpty and RegexConcat are not contained, RegexStringLiteral do not appear consecutive
                | RegexUnion (Set Regex)                  -- invariant: number of elements is at least 2, elements are unique, RegexUnion is not contained
                | RegexLoop Regex Integer (Maybe Integer) -- invariant: loops are not directly nested
                | RegexRange Char Char                    -- invariant lowerbound < upperbound
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Regex: regular expression
--
-- 1. User can't directly construct Regex (such that invariants will always hold)
--
-- 2. User can still pattern match on Regex using 'RegexView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype Regex = Regex { -- | View on regular expression.
                        view :: RegexView
                      }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | regexRangeLow: lowest character in the regular expression range
regexRangeLow :: Char
regexRangeLow = '\x00'

-- | regexRangeHigh: highest character in the regular expression range
regexRangeHigh :: Char
regexRangeHigh = '\xFF'

-- | constructor for the Regular Expression that only contains the provided string.
mkRegexStringLiteral :: Text -> Regex
mkRegexStringLiteral = Regex . RegexStringLiteral

-- | constructor for the Regular Expression that concatinates the provided list of regular expressions
mkRegexConcat :: [Regex] -> Regex
mkRegexConcat ls = case ( merge . flatten . Prelude.filter (mkRegexEmpty /=) ) ls of    -- on the order: don't filter already checked regex concatinations
                        []  -> mkRegexEmpty
                        [x] -> x
                        fs  -> Regex $ RegexConcat fs
    where
        merge :: [Regex] -> [Regex]
        merge []                                = []
        merge [x]                               = [x]
        merge ( (view -> RegexStringLiteral t1)
              : (view -> RegexStringLiteral t2)
              : xs)                             = merge (Regex (RegexStringLiteral (append t1 t2)) : xs)
        merge (x1:x2:xs)                        = x1 : merge (x2:xs)

        flatten :: [Regex] -> [Regex]
        flatten = Prelude.concatMap fromRegex

        fromRegex :: Regex -> [Regex]
        fromRegex (view -> RegexConcat cs) = cs
        fromRegex x                        = [x]

-- | constructor for the Regular Expression that unions the provided list of regular expressions.
--
-- precondition: the list is not empty
-- This precondition is needed since Posix doesn't support a concept like SMT's re.nostr.
mkRegexUnion :: [Regex] -> Either Error Regex
mkRegexUnion ls = let set = flatten ls in
                    case Data.Set.toList set of
                        []  -> Left $ Error ("precondition violation: input is empty list")
                        [x] -> Right $ x
                        _   -> Right $ Regex (RegexUnion set)
    where
        flatten :: [Regex] -> Set Regex
        flatten = Data.Set.unions . Prelude.map fromRegex

        fromRegex :: Regex -> Set Regex
        fromRegex (view -> RegexUnion us) = us
        fromRegex x                       = Data.Set.singleton x

-- | constructor for the Regular Expression that loops the provided regular expression
-- a regular expression that contains at least `lowerbound` repetitions of `basis` and, when present, at most `upperbound` repetitions of `basis`.
--
-- preconditions: lowerbound >= 0
--                when present: upperbound >= lowerbound
mkRegexLoop :: Regex         -- ^ basis regular expression
            -> Integer       -- ^ lowerbound
            -> Maybe Integer -- ^ optional upperbound
            -> Either Error Regex
mkRegexLoop _ l _        | l < 0 = Left $ Error ("precondition violation: lowerbound (" ++ show l ++ ") is negative.")
mkRegexLoop _ l (Just u) | u < l = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is smaller than lowerbound (" ++ show l ++ ").")
mkRegexLoop _ 0 (Just 0)         = Right $ mkRegexEmpty
mkRegexLoop r 1 (Just 1)         = Right $ r
-- optimization: Loop of string literal with lowerbound == upperbound is just string literal * lowerbound
mkRegexLoop r l mu               = Right $ mkRegexViewLoop (view r)
    where
        mkRegexViewLoop :: RegexView -> Regex
        mkRegexViewLoop (RegexLoop ri li mui) = let nu = case (mu, mui) of
                                                            (Nothing, Nothing) -> Nothing
                                                            (Nothing, Just _ ) -> Nothing
                                                            (Just _ , Nothing) -> Nothing
                                                            (Just u , Just ui) -> Just (u * ui)
                                                    in
                                                        Regex (RegexLoop ri (l * li) nu)
        mkRegexViewLoop _                     = Regex (RegexLoop r l mu)

-- | constructor for the Regular Expression that contains all characters in the provided range.
--
--  precondition: lowerbound <= upperbound
mkRegexRange :: Char -- ^ lowerbound
             -> Char -- ^ upperbound
             -> Either Error Regex
mkRegexRange l _ | l < regexRangeLow  = Left $ Error ("precondition violation: lowerbound (" ++ show l ++ ") is smaller than lowest character in range (" ++ show regexRangeLow ++ ").")
mkRegexRange _ u | regexRangeHigh < u = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is larger than highest character in range (" ++ show regexRangeHigh ++ ").")
mkRegexRange l u | u < l              = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is smaller than lowerbound (" ++ show l ++ ").")
mkRegexRange l u | u == l             = Right $ mkRegexStringLiteral (Data.Text.singleton l)
mkRegexRange l u                      = Right $ Regex (RegexRange l u)

-- | constructor for the Regular Expression that contains all characters.
mkRegexAll :: Regex
mkRegexAll = case mkRegexRange regexRangeLow regexRangeHigh of
                Left e  -> error ("mkRegexRange failed unexpectedly for mkRegexAll with " ++ show e)
                Right x -> x

-- | constructor for the optional Regular Expression (?)
mkRegexOptional :: Regex -> Regex
mkRegexOptional r = case mkRegexLoop r 0 (Just 1) of
                        Left e  -> error ("mkRegexLoop failed unexpectedly for mkRegexOptional with " ++ show e)
                        Right o -> o

-- | constructor for the Regular Expression Kleene Star (*)
mkRegexKleeneStar :: Regex -> Regex
mkRegexKleeneStar r = case mkRegexLoop r 0 Nothing of
                        Left e  -> error ("mkRegexLoop failed unexpectedly for mkRegexKleeneStar with " ++ show e)
                        Right s -> s

-- | constructor for the Regular Expression Kleene Cross (+)
mkRegexKleeneCross :: Regex -> Regex
mkRegexKleeneCross r = case mkRegexLoop r 1 Nothing of
                        Left e  -> error ("mkRegexLoop failed unexpectedly for mkRegexKleeneCross with " ++ show e)
                        Right c -> c

-- | constructor for the Regular Expression Dot (.).
-- all characters except new line characters
mkRegexDot :: Regex
mkRegexDot = let ranges = [ mkRegexRange '\x00' '\x09'
                         , mkRegexRange '\x0B' '\x0C'
                         , mkRegexRange '\x0E' '\xFF'
                         ] in
                 case partitionEithers ranges of
                    ([], rs)    -> case mkRegexUnion rs of
                                    Left e -> error ("mkRegexDot failed unexpectedly on union with " ++ show e)
                                    Right r -> r
                    (es, _)     -> error ("mkRegexDot failed unexpectedly on ranges with " ++ show es)

-- | constructor for the Regular Expression that only contains the empty string.
mkRegexEmpty :: Regex
mkRegexEmpty = mkRegexStringLiteral Data.Text.empty
