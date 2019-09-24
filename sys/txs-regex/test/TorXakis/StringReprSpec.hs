{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.StringReprSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Regex'.
-----------------------------------------------------------------------------
module TorXakis.StringReprSpec
(spec
)
where
import qualified Data.Text
import           Test.Hspec

import           TorXakis.Regex
import qualified TorXakis.Regex.CharRepr
import qualified TorXakis.Regex.StringRepr

expectationCharRepr :: Expectation
expectationCharRepr =
    case mkRegexStringLiteral (Data.Text.pack "aab") of
        Left e  -> error ("mkRegexStringLiteral unexpectedly failed with " ++ show e)
        Right r -> TorXakis.Regex.CharRepr.viewCharRepr r `shouldBe` expected
  where
    expected :: TorXakis.Regex.CharRepr.CharRepr
    expected = TorXakis.Regex.CharRepr.RegexConcat [ TorXakis.Regex.CharRepr.RegexLoop (TorXakis.Regex.CharRepr.RegexCharLiteral 'a') 2 (Just 2)
                                                   , TorXakis.Regex.CharRepr.RegexCharLiteral 'b'
                                                   ]

expectationStringRepr :: Expectation
expectationStringRepr =
    let t = Data.Text.pack "aab" in
    case mkRegexStringLiteral t of
        Left e  -> error ("mkRegexStringLiteral unexpectedly failed with " ++ show e)
        Right r -> TorXakis.Regex.StringRepr.viewStringRepr r `shouldBe` TorXakis.Regex.StringRepr.RegexStringLiteral t

spec :: Spec
spec =
  describe "A Regex has multiple representations" $ do
    it "has a character based representation" expectationCharRepr
    it "has a string based representation" expectationStringRepr