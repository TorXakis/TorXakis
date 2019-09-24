{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.RegexBasis
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
module TorXakis.Regex.RegexBasis
( -- * Allowed character range
  regexRangeLow
, regexRangeHigh
  -- * Constructors
, mkRegexEmpty
, mkRegexCharLiteral
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
, Regex
)
where
import           Control.Exception   (assert)
import           Data.Either
import qualified Data.List
import           Data.Set
import qualified Data.Text

import           TorXakis.Error
import           TorXakis.Regex.Regex

-- TODO: Make canonical form even stronger
-- see https://stackoverflow.com/questions/3123748/how-can-i-normalize-canonize-a-regular-expression-pattern
--     https://cs.stackexchange.com/questions/88682/how-do-you-convert-a-regular-expression-to-its-disjunctive-normal-form
-- for hints

-- | regexRangeLow: lowest character in the regular expression range
regexRangeLow :: Char
regexRangeLow = '\x00'

-- | regexRangeHigh: highest character in the regular expression range
regexRangeHigh :: Char
regexRangeHigh = '\xFF'

-- | constructor for the Regular Expression that only contains the empty string.
mkRegexEmpty :: Regex
mkRegexEmpty = RegexEmpty

-- | constructor for the Regular Expression that only contains the given character.
mkRegexCharLiteral :: Char -> Either Error Regex
mkRegexCharLiteral c | regexRangeLow <= c && c <= regexRangeHigh = Right $ RegexCharLiteral c
                     | otherwise                                 = Left $ Error ("precondition violation: character "++ show c ++ " outside [" ++ show regexRangeLow ++ ", " ++ show regexRangeHigh ++ "].")

-- | constructor for the Regular Expression that contains all characters in the provided range.
--
--  precondition: lowerbound <= upperbound
mkRegexRange :: Char -- ^ lowerbound
             -> Char -- ^ upperbound
             -> Either Error Regex
mkRegexRange l _ | l < regexRangeLow  = Left $ Error ("precondition violation: lowerbound (" ++ show l ++ ") is smaller than lowest character in range (" ++ show regexRangeLow ++ ").")
mkRegexRange _ u | regexRangeHigh < u = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is larger than highest character in range (" ++ show regexRangeHigh ++ ").")
mkRegexRange l u | u < l              = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is smaller than lowerbound (" ++ show l ++ ").")
mkRegexRange l u | u == l             = mkRegexCharLiteral l
mkRegexRange l u                      = Right $ RegexRange l u

-- | constructor for the Regular Expression that concatinates the provided list of regular expressions
mkRegexConcat :: [Regex] -> Regex
mkRegexConcat ls = case ( merge . flatten . Prelude.filter (mkRegexEmpty /=) ) ls of    -- on the order: don't filter already checked regex concatinations
                        []  -> mkRegexEmpty
                        [x] -> x
                        fs  -> RegexConcat fs
    where
        merge :: [Regex] -> [Regex]
        merge []                                    = []
        merge [r]                                   = [r]
        merge ( r1
              : r2
              : rs)  | r1 == r2                     = case mkRegexLoop r1 2 (Just 2) of
                                                            Left e -> error ("mkRegexLoop unexpectedly failed with " ++ show e)
                                                            Right r -> merge (r:rs)
        merge ( RegexLoop r1 l mu
              : r2 
              : rs ) | r1 == r2                     = let nu = case mu of
                                                                    Nothing -> Nothing
                                                                    Just u  -> Just (u + 1)
                                                        in
                                                            merge (RegexLoop r1 (l+1) nu : rs)
        merge ( r1
              : RegexLoop r2 l mu 
              : rs ) | r1 == r2                     = let nu = case mu of
                                                                    Nothing -> Nothing
                                                                    Just u  -> Just (u + 1)
                                                        in
                                                            merge (RegexLoop r1 (l+1) nu : rs)
        merge ( RegexLoop r@(RegexConcat cs) l mu
              : rs ) | cs `Data.List.isPrefixOf` rs = let nu = case mu of
                                                                    Nothing -> Nothing
                                                                    Just u  -> Just (u + 1)
                                                        in
                                                            merge (RegexLoop r (l+1) nu : dropPrefix cs rs)
        merge ( RegexLoop r1 l1 mu1
              : RegexLoop r2 l2 mu2
              : rs)  | r1 == r2                     = let nu = case (mu1, mu2) of    -- combine loops   x{1,2}x{3,4} == x{4,6}
                                                            (Nothing, Nothing) -> Nothing
                                                            (Nothing, Just _ ) -> Nothing
                                                            (Just _ , Nothing) -> Nothing
                                                            (Just u1, Just u2) -> Just (u1 + u2)
                                                        in
                                                            merge (RegexLoop r1 (l1+l2) nu : rs)
        merge (r1:r2:rs)                            = r1 : merge (r2:rs)

        flatten :: [Regex] -> [Regex]
        flatten = Prelude.concatMap fromRegex

        fromRegex :: Regex -> [Regex]
        fromRegex (RegexConcat rs) = rs
        fromRegex  r               = [r]

        dropPrefix :: Eq a => [a] -> [a] -> [a]
        dropPrefix [] ys            = ys
        dropPrefix _  []            = error "Argument passed as prefix is not a prefix"
        dropPrefix (x:xs) (y:ys)    = assert (x==y) $ dropPrefix xs ys

-- | constructor for the Regular Expression that unions the provided list of regular expressions.
--
-- precondition: the list is not empty
-- This precondition is needed since Posix doesn't support a concept like SMT's re.nostr.
mkRegexUnion :: [Regex] -> Either Error Regex
mkRegexUnion ls = -- TODO: merge overlapping / adjacent ranges in union e.g. [A-KL-Z] == [A-Z]
                  -- TODO: merge similar concat starts e.g. (xyza)|(xyzb)|(xyzc) == xyz(a|b|c)
                  let set = flatten ls in
                    case Data.Set.toList set of
                        []  -> Left $ Error "precondition violation: input is empty list"
                        [x] -> Right x
                        _   -> Right $ RegexUnion set
    where
        flatten :: [Regex] -> Set Regex
        flatten = Data.Set.unions . Prelude.map fromRegex

        fromRegex :: Regex -> Set Regex
        fromRegex (RegexUnion rs) = rs
        fromRegex  r              = Data.Set.singleton r

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
mkRegexLoop _ 0 (Just 0)         = Right mkRegexEmpty  -- also ensures that "(a{20,}){0,0}" == "()" and NOT "a{0,}"
mkRegexLoop r 1 (Just 1)         = Right r
mkRegexLoop r l mu               = Right $ mkRegexInnerLoop r
    where
        mkRegexInnerLoop :: Regex -> Regex
        mkRegexInnerLoop  RegexEmpty                            = RegexEmpty
        mkRegexInnerLoop (RegexLoop ri li mui) | mui /= Just li = -- combine loops but not innerloops with no choice 
                                                                  -- since (a{5,5}){2,3} <> a{10,15}
                                                                   let nu = case (mu, mui) of    -- combine loops
                                                                
                                                                               (Nothing, Nothing) -> Nothing
                                                                               (Nothing, Just _ ) -> Nothing
                                                                               (Just _ , Nothing) -> Nothing
                                                                               (Just u , Just ui) -> Just (u * ui)
                                                                       in
                                                                           RegexLoop ri (l * li) nu
        mkRegexInnerLoop _                                      = RegexLoop r l mu

-- | constructor for the Regular Expression that only contains the provided string.
mkRegexStringLiteral :: Data.Text.Text -> Either Error Regex
mkRegexStringLiteral t = case partitionEithers (Data.List.map mkRegexCharLiteral (Data.Text.unpack t)) of
                            ([], cs) -> Right $ mkRegexConcat cs
                            (es, _ ) -> Left $ Error ("Illegal characters in string\n" ++ show es)

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
