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
import           Test.Hspec
import           Test.QuickCheck
import           Text.Regex.TDFA

-- This empty test case learns us that at top level posix needs brackets or anchors:
-- When using "" we get the following error
--
-- Explict error in module Text.Regex.TDFA.String : Text.Regex.TDFA.String died: parseRegex for Text.Regex.TDFA.String failed:(line 1, column 1):
--       unexpected end of input
--       expecting empty () or anchor ^ or $ or an atom
--
prop_RegexEmpty :: Bool
prop_RegexEmpty =
            ("" =~"()")
      &&    ("" =~"^$")
      &&    ("" =~"\\`\\'")

-- posix has equivalent notations, which we exploit to support one union operator only (like regexes in SMT)
prop_Equivalent :: String -> Bool
prop_Equivalent txt =
    let result1 :: Bool
        result1 = txt =~ "[A-Za-z_]+"
        result2 :: Bool
        result2 = txt =~ "([A-Z]|[a-z]|_)+" in
      result1 == result2
      
spec :: Spec
spec = 
  describe "A Regex" $ do
    it "can be satisfied by an empty string" $ property prop_RegexEmpty
    it "has equivalent representations" $ property prop_Equivalent