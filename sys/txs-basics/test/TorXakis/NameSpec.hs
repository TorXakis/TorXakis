{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ErrorSpec
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
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Name

prop_empty :: Bool
prop_empty =
    case mkName (T.pack "") of
        Left _  -> True
        Right _ -> False

prop_nonEmpty :: Char -> String -> Bool
prop_nonEmpty c s =
    let txt = T.pack (c:s) in
        case mkName txt of
            Left _  -> False
            Right n -> txt == toText n


spec :: Spec
spec = 
  describe "mkName (the smart constructor of Name)"$ do
    it "doesn't accept empty string" $ property prop_empty
    it "does accept any non-empty string" $ property prop_nonEmpty
