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
import           Control.Monad.IO.Class
import           Data.Text
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Regex.TDFA

import           TorXakis.Regex
import           TorXakis.RegexGen
import           TorXakis.StringFromRegex

-- | stringFromRegex 
prop_StringFromRegex :: RegexGen -> Property
prop_StringFromRegex (RegexGen r) = monadicIO $ do
    s <- liftIO $ stringFromRegex r
    assert $ unpack s =~ unpack (toPosix r)


spec :: Spec
spec =
  describe "String From Regex" $ 
        it "adheres to the regex" $ property prop_StringFromRegex
