{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RegexSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Regex'.
-----------------------------------------------------------------------------
module TorXakis.RegexSpec
(spec
)
where
import qualified Control.Exception
import           Control.Monad.IO.Class
import           Data.Char
import qualified Data.Text
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Regex.TDFA ((=~))

import           TorXakis.Regex
import           TorXakis.Regex.Posix

-- This empty test case learns us that at top level posix needs brackets or anchors:
-- When using "" we get the following error
--
-- Explict error in module Text.Regex.TDFA.String : Text.Regex.TDFA.String died: parseRegex for Text.Regex.TDFA.String failed:(line 1, column 1):
--       unexpected end of input
--       expecting empty () or anchor ^ or $ or an atom
--
test_RegexEmpty :: Bool
test_RegexEmpty =
            ("" =~ "()")
      &&    ("" =~ "^$")
      &&    ("" =~ "\\`\\'")

-- | We must work around this error in ranged
test_Ranges :: Property
test_Ranges = monadicIO $
    liftIO $ Control.Exception.catch (Control.Exception.evaluate $ "A" =~ "[--@]")
                                                                    -- A is outside range so False should be produced, unless an error occurs
                                                                    -- see https://github.com/ChrisKuklewicz/regex-tdfa/issues/24
                                     handler
  where
    handler :: Control.Exception.ErrorCall -> IO Bool
    handler = const (return True)

-- | all Char in Regex Range
allChars :: String
allChars = [regexRangeLow..regexRangeHigh]

-- | Are all chars correctly handled (by escaping when needed)
test_Chars :: Bool
test_Chars =
        all matchCharRegex allChars
    where
        matchCharRegex :: Char -> Bool
        matchCharRegex c = [c] =~ posixRegex
            where
                posixRegex = case mkRegexCharLiteral c of
                                Right r -> Data.Text.unpack (toPosix r)
                                Left e  -> error ("mkRegexCharLiteral unexpectedly failed with "++ show e)

-- | Is Char in range of low and high
inRange :: Char -> Char -> Char -> Bool
inRange l h x = [x] =~ case mkRegexRange l h of
                                Left e -> error ("Unexpected failure of mkRegexRange with "++ show e)
                                Right r -> Data.Text.unpack (toPosix r)

-- | Are all chars correctly handled (by escaping when needed) in the lowerbound Position of a range
-- In posix, the character at the position of u at [l - u]
test_CharsLowRange :: Bool
test_CharsLowRange =
       all matchRanges allChars
    where
        matchRanges :: Char -> Bool
        matchRanges c = let (invalidRange, validRange) = splitAt (Data.Char.ord c - Data.Char.ord regexRangeLow) allChars
                            in
                                (      all (inRange c regexRangeHigh) validRange
                                    && all (not . inRange c regexRangeHigh) invalidRange
                                )
                                || trace ("Char " ++ show c ++ " fails") False



-- | Are all chars correctly handled (by escaping when needed) in the Upperbound Position of a range
-- In posix, the character at the position of u at [l - u]
test_CharsHighRange :: Bool
test_CharsHighRange =
       all matchRanges allChars
    where
        matchRanges c = let (validRange, invalidRange) = splitAt (Data.Char.ord c - Data.Char.ord regexRangeLow + 1) allChars
                            in
                                (      all (inRange regexRangeLow c) validRange
                                    && all (not . inRange regexRangeLow c) invalidRange
                                )
                                || trace ("Char " ++ show c ++ " fails") False

-- | generic equivalent regex function
prop_Equivalent :: String -> String -> String -> Bool
prop_Equivalent posix1 posix2 txt = 
    let result1 :: Bool
        result1 = txt =~ posix1
        result2 :: Bool
        result2 = txt =~ posix2
      in
        result1 == result2

-- | posix has equivalent notations, which we exploit to support one union operator only (like regexes in SMT)
prop_Equivalent_UnionInCharGroups :: String -> Bool
prop_Equivalent_UnionInCharGroups = prop_Equivalent "[A-Za-z_]+" "([A-Z]|[a-z]|_)+"

-- | Empty Regex in loop is just empty regex
prop_Equivalent_EmptyInLoop :: String -> Bool
prop_Equivalent_EmptyInLoop = prop_Equivalent "(){3,6}" "()"

-- | Loop with zero occurances is empty regex
prop_Equivalent_LoopZeroIsEmpty :: String -> Bool
prop_Equivalent_LoopZeroIsEmpty = prop_Equivalent "(abcdef){0,0}" "()"

-- | Loop with one occurance is just that regex
prop_Equivalent_LoopOnceIsIdentity :: String -> Bool
prop_Equivalent_LoopOnceIsIdentity = prop_Equivalent "([A-Z]+){1,1}" "[A-Z]+"

-- | Empty in concat is removable
prop_Equivalent_EmptyInConcatRemovable :: String -> Bool
prop_Equivalent_EmptyInConcatRemovable = prop_Equivalent "()[A-Z]()[a-z]()" "[A-Z][a-z]"

-- | A singleton range is allowed and equal to that char
prop_Equivalent_SingletonRange :: String -> Bool
prop_Equivalent_SingletonRange = prop_Equivalent "[A-A]*" "A*"

-- | nested unions can be flattened
prop_Equivalent_UnionNested :: String -> Bool
prop_Equivalent_UnionNested = prop_Equivalent "((a|b)|(c|d))+" "(a|b|c|d)+"

-- | nested concats can be flattened
prop_Equivalent_ConcatNested :: String -> Bool
prop_Equivalent_ConcatNested = prop_Equivalent "(((a)(b))((c)(d)))+" "(abcd)+"

-- | concat and unions can be distributed
prop_Equivalent_UnionConcat :: String -> Bool
prop_Equivalent_UnionConcat = prop_Equivalent "(z)|(za)|(zb)" "(z)(()|a|b)"

-- | nested loops can be flattened
prop_Equivalent_LoopNested :: String -> Bool
prop_Equivalent_LoopNested = prop_Equivalent "(a{3,4}){6,7}" "a{18,28}"

-- | concatenated loops over the same regular expression can be combined
prop_Equivalent_LoopConcatenated :: String -> Bool
prop_Equivalent_LoopConcatenated = prop_Equivalent "(a{1,2})(a{2,3})" "a{3,5}"

-- | fixed loops of strings can be simplified
prop_Equivalent_FixedLoop :: String -> Bool
prop_Equivalent_FixedLoop = prop_Equivalent "a{2,2}" "aa"

-- | instances and loops over them can be combined
-- rewritting "aa{4,7}" to "a{5,8}" doesn't always simplify the str.in.re problem.
-- Yet in some contexts it does! For example, rewritting "a{3,6}aa{4,7}" to "a{8,14}" simplifies the str.in.re problem.
-- Note when a concatenated regex is in a loop, we must look at the front/tail of any concatenated regex after/before it.
prop_Equivalent_LoopAndInstance :: String -> Bool
prop_Equivalent_LoopAndInstance = prop_Equivalent "(jaap)(aap)*" "j(aap)+"

-- | adjacent ranges can be combined
prop_Equivalent_AdjacentRanges :: String -> Bool
prop_Equivalent_AdjacentRanges = prop_Equivalent "([A-K]|[L-Z])*" "([A-Z])*"

-- | overlapping ranges can be combined
prop_Equivalent_OverlappingRanges :: String -> Bool
prop_Equivalent_OverlappingRanges = prop_Equivalent "([A-P]|[L-Z])*" "([A-Z])*"

-- | ranges and characters can be combined
prop_Equivalent_CharAdjacentRange :: String -> Bool
prop_Equivalent_CharAdjacentRange = prop_Equivalent "(A|[B-Z])*" "([A-Z])*"

-- | loop can be unfolded and differently refolded with adjacent parts
-- "(ab){3,4}a(ba){2,3}" can be rewritten to the easier regex (to solve) "(ab){5,7}a"
prop_Equivalent_LoopRefold :: String -> Bool
prop_Equivalent_LoopRefold = prop_Equivalent "A(BA)*" "(AB)*A"

spec :: Spec
spec = do
  describe "A Regex" $ do
    it "can be satisfied by an empty string" test_RegexEmpty
    it "handle special ranges correctly" test_Ranges
    it "handle chars correctly" test_Chars
    it "handle chars correctly in lowerbound position of a range" test_CharsLowRange
    it "handle chars correctly in upperbound position of a range" test_CharsHighRange
  describe "A regex has equivalent representations for" $ do
    it "union in character groups" $ property prop_Equivalent_UnionInCharGroups
    it "empty in loop is empty" $ property prop_Equivalent_EmptyInLoop
    it "loop zero times is empty" $ property prop_Equivalent_LoopZeroIsEmpty
    it "loop once is identity" $ property prop_Equivalent_LoopOnceIsIdentity
    it "empty can be removed in concat" $ property prop_Equivalent_EmptyInConcatRemovable
    it "singleton range is character" $ property prop_Equivalent_SingletonRange
    it "nested unions" $ property prop_Equivalent_UnionNested
    it "nested concats" $ property prop_Equivalent_ConcatNested
    it "concat and unions" $ property prop_Equivalent_UnionConcat
    it "nested loops" $ property prop_Equivalent_LoopNested
    it "concatenate loops over same regex" $ property prop_Equivalent_LoopConcatenated
    it "fixed loops of text" $ property prop_Equivalent_FixedLoop
    it "combine instance and loop of that instance" $ property prop_Equivalent_LoopAndInstance
    it "combine adjacent ranges" $ property prop_Equivalent_AdjacentRanges
    it "combine overlapping ranges" $ property prop_Equivalent_OverlappingRanges
    it "combine char and adjacent range" $ property prop_Equivalent_CharAdjacentRange
    it "loops including adjacent elements" $ property prop_Equivalent_LoopRefold