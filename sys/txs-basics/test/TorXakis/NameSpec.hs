{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NameSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Name'.
-----------------------------------------------------------------------------
module TorXakis.NameSpec
(spec
)
where
import           Data.Either
import qualified Data.Text          as T
import           Text.Regex.TDFA

import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Name

prop_empty :: Bool
prop_empty =
    case mkName (T.pack "") of
        Left _  -> True
        Right _ -> False

-- | match regex
-- note ^ and $ are line boundaries, so "\na" matches "^[A-Z_a-z][A-Z_a-z0-9-]*$".
-- we need buffer boundaries!
-- However \A and \z (see https://www.boost.org/doc/libs/1_44_0/libs/regex/doc/html/boost_regex/syntax/basic_extended.html)
-- are not supported, so we need lines to ensure we have a single line and 
-- thus line boundaries are buffer boundaries.
prop_regex :: String -> Bool
prop_regex str =
    let txt = T.pack str 
      in case lines str of
        [content] -> if content =~ "^[A-Z_a-z][A-Z_a-z0-9-]*$"
                        then isRight $ mkName txt
                        else isLeft $ mkName txt
        _         -> isLeft $ mkName txt

spec :: Spec
spec = 
  describe "mkName (the smart constructor of Name)"$ do
    it "doesn't accept empty string" $ property prop_empty
    it "does accept strings that adhere to regex" $ property prop_regex
