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
-- Test specifications for 'Error'.
-----------------------------------------------------------------------------
module TorXakis.ErrorSpec
(spec
)
where
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Error

prop_toText :: String -> Bool
prop_toText s =
    let txt = T.pack s in 
      txt == toText (Error txt)

spec :: Spec
spec = 
  describe "A Error"$
    it "contains a text" $ property prop_toText
