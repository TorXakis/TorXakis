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
import           Debug.Trace
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext
import           TorXakis.Value
import           TorXakis.ValueGen

-- | ConversionText is Identity
prop_ConversionText_id :: Gen Bool
prop_ConversionText_id = 
    let c0 = empty :: MinimalTestSortContext in do
        incr1 <- arbitraryADTDefs c0
        case addAdtDefs c0 incr1 of
            Left e  -> error ("Invalid generator - " ++ show e)
            Right ctx -> prop_Ctx_ConversionText_id ctx

prop_Ctx_ConversionText_id :: TestSortContext a => a -> Gen Bool
prop_Ctx_ConversionText_id ctx =
    do
        vals <- listOf1 (arbitraryValue ctx)
        return $ all check vals
    where check :: Value -> Bool
          check v = 
                let txt = valueToText ctx v
                    actual = valueFromText ctx (getSort v) txt
                  in
                    trace ("txt = " ++ show txt) $
                        case actual of
                            Left e   -> trace ("\nParse error " ++ show e) False
                            Right v' -> v == v'

-- | ConversionXML is Identity
prop_ConversionXML_id :: Gen Bool
prop_ConversionXML_id = 
    let c0 = empty :: MinimalTestSortContext in do
        incr1 <- arbitraryADTDefs c0
        case addAdtDefs c0 incr1 of
            Left e  -> error ("Invalid generator - " ++ show e)
            Right ctx -> prop_Ctx_ConversionXML_id ctx

prop_Ctx_ConversionXML_id :: TestSortContext a => a -> Gen Bool
prop_Ctx_ConversionXML_id ctx =
    do
        vals <- listOf1 (arbitraryValue ctx)
        return $ all check vals
    where check :: Value -> Bool
          check v = 
                let xml = valueToXML ctx v
                    actual = valueFromXML ctx (getSort v) xml
                  in
                    trace ("xml = " ++ show xml) $
                        case actual of
                            Left e   -> trace ("\nParse error " ++ show e) False
                            Right v' -> v == v'

spec :: Spec
spec =
  describe "conversion" $
    modifyMaxSuccess (const 50) $
    modifyMaxSize (const 10) $ 
      do
        it "fromText . toText == id" $ property prop_ConversionText_id
        it "fromXML . toXML == id" $ property prop_ConversionXML_id
