{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex
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
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ViewPatterns          #-}
module TorXakis.Regex
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
, mkRegexRange
, mkRegexOptional
, mkRegexKleeneStar
, mkRegexKleeneCross
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
data RegexView =  RegexEmpty
                | RegexStringLiteral Text       -- invariant: Text is not null
                | RegexConcat [Regex]           -- invariant: length list is at least 2, RegexEmpty and RegexConcat are not contained, RegexStringLiteral do not appear consecutive
                | RegexUnion (Set Regex)        -- invariant: number of elements is at least 2, elements are unique, RegexEmpty and RegexUnion are not contained
                | RegexLoop Regex Integer (Maybe Integer) -- invariant: loops are not directly nested
                | RegexRange Char Char
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

-- | constructor for the Empty Regular Expression
mkRegexEmpty :: Regex
mkRegexEmpty = Regex RegexEmpty

-- | constructor for the Regular Expression that only contains the provided string.
mkRegexStringLiteral :: Text -> Regex
mkRegexStringLiteral t | Data.Text.null t = mkRegexEmpty
mkRegexStringLiteral t                    = Regex $ RegexStringLiteral t

-- | constructor for the Regular Expression that concats the provided list of regular expressions
mkRegexConcat :: [Regex] -> Regex
mkRegexConcat ls = case (merge . flatten . Prelude.filter notRegexEmpty) ls of
                        []  -> mkRegexEmpty
                        [x] -> x
                        fs  -> Regex $ RegexConcat fs
    where
        merge :: [Regex] -> [Regex]
        merge []  = []
        merge [x] = [x]
        merge ( (view -> RegexStringLiteral t1)
              : (view -> RegexStringLiteral t2)
              : xs)                             = merge (Regex (RegexStringLiteral (append t1 t2)) : xs)
        merge (x1:x2:xs)                        = x1 : merge (x2:xs)

        flatten :: [Regex] -> [Regex]
        flatten = Prelude.concatMap fromRegex

        fromRegex :: Regex -> [Regex]
        fromRegex (view -> RegexConcat cs) = cs
        fromRegex x                        = [x]

        notRegexEmpty :: Regex -> Bool
        notRegexEmpty r = case view r of
                            RegexEmpty -> False
                            _          -> True

-- | constructor for the Regular Expression that unions the provided list of regular expressions
mkRegexUnion :: [Regex] -> Regex
mkRegexUnion ls = let set = (Data.Set.delete mkRegexEmpty . flatten) ls in
                    case Data.Set.toList set of
                        []  -> mkRegexEmpty
                        [x] -> x
                        _   -> Regex $ RegexUnion set
    where
        flatten :: [Regex] -> Set Regex
        flatten = Data.Set.unions . Prelude.map fromRegex

        fromRegex :: Regex -> Set Regex
        fromRegex (view -> RegexUnion us) = us
        fromRegex x                       = Data.Set.singleton x


-- | constructor for the Regular Expression that loops the provided regular expression
-- a regular expression that contains at least `lowerbound` repetitions of `basis` and, when present, at most `upperbound` repetitions of `basis`.
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

-- | constructor for the Regular Expression that contains of all character in the provided range.
--  precondition: lowerbound <= upperbound
mkRegexRange :: Char -- ^ lowerbound
             -> Char -- ^ upperbound
             -> Either Error Regex
mkRegexRange l u | u < l  = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is smaller than lowerbound (" ++ show l ++ ").")
mkRegexRange l u | u == l = Right $ Regex (RegexStringLiteral (Data.Text.singleton l))
mkRegexRange l u          = Right $ Regex (RegexRange l u)

-- | constructor for the Regular Expression that contains of all character.
mkRegexAll :: Regex
mkRegexAll = case mkRegexRange regexRangeLow regexRangeHigh of
                Left e  -> error ("mkRegexRange failed unexpectedly with " ++ show e)
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

-- | constructor for the Regular Expression Dot (.)
-- all characters except new line characters
mkRegexDot :: Regex
mkRegexDot = let ranges = [ mkRegexRange '\x00' '\x09'
                          , mkRegexRange '\x0B' '\x0C'
                          , mkRegexRange '\x0E' '\xFF'
                          ] in
                 case partitionEithers ranges of
                    ([], rs)    -> mkRegexUnion rs
                    (es, _)     -> error ("mkRegexDot failed unexpectedly on range with " ++ show es)
