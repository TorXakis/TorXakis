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
import           Text.Regex.TDFA

import           TorXakis.Regex

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

-- posix has equivalent notations, which we exploit to support one union operator only (like regexes in SMT)
prop_Equivalent :: String -> Bool
prop_Equivalent txt =
    let result1 :: Bool
        result1 = txt =~ "[A-Za-z_]+"
        result2 :: Bool
        result2 = txt =~ "([A-Z]|[a-z]|_)+" in
      result1 == result2

-- | all Char in Regex Range
allChars :: [Char]
allChars = [regexRangeLow..regexRangeHigh]

-- | Are all chars correctly handled (by escaping when needed)
test_Chars :: Bool
test_Chars =
        all matchRegex allChars
    where
        matchRegex :: Char -> Bool
        matchRegex c = [c] =~ Data.Text.unpack (toPosix (mkRegexStringLiteral (Data.Text.singleton c)))

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
                                (     (all (inRange c regexRangeHigh) validRange)
                                && (all (not . inRange c regexRangeHigh) invalidRange)
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
                                (      (all (inRange regexRangeLow c) validRange)
                                    && (all (not . inRange regexRangeLow c) invalidRange)
                                )
                                || trace ("Char " ++ show c ++ " fails") False

spec :: Spec
spec =
  describe "A Regex" $ do
    it "can be satisfied by an empty string" test_RegexEmpty
    it "handle special ranges correctly" test_Ranges
    it "has equivalent representations" $ property prop_Equivalent
    it "handle chars correctly" test_Chars
    it "handle chars correctly in lowerbound position of a range" test_CharsLowRange
    it "handle chars correctly in upperbound position of a range" test_CharsHighRange
