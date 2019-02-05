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
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Error

prop_show :: String -> Bool
prop_show txt =
      show txt == show (Error txt)

spec :: Spec
spec = 
  describe "A Error" $
    it "can be shown" $ property prop_show
