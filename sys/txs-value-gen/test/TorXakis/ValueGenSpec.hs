{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValueGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortGenContext'.
-----------------------------------------------------------------------------
module TorXakis.ValueGenSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Value
import           TorXakis.ValueGen

-- | ConversionText is Identity
prop_ConversionText_id :: Gen Bool
prop_ConversionText_id = 
    let c0 = empty :: MinimalTestSortContext in do
        incr1 <- arbitraryADTDefs c0
        case addAdtDefs c0 incr1 of
            Left e  -> error ("Invalid generator - " ++ show e)
            Right ctx -> do
                            val <- arbitraryValue ctx
                            return $ val == valueFromText ctx (valueToText ctx val)

spec :: Spec
spec =
  describe "conversion" $
    it "fromText . toText == id" $ property prop_ConversionText_id
