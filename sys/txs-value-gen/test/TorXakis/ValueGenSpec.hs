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

propertyInContext  :: (MinimalTestSortContext -> Gen Bool) -> Gen Bool
propertyInContext prop = 
    let c0 = empty :: MinimalTestSortContext in do
        incr1 <- arbitraryADTDefs c0
        case addAdtDefs c0 incr1 of
            Left e    -> error ("Invalid generator - " ++ show e)
            Right ctx -> prop ctx


-- | ConversionText is Identity
prop_ConversionText_id :: TestSortContext a => a -> Gen Bool
prop_ConversionText_id ctx = all check <$> listOf1 (arbitraryValue ctx)
    where check :: Value -> Bool
          check v = 
                let txt = valueToText ctx v
                    actual = valueFromText ctx (getSort v) txt
                  in
                        case actual of
                            Left e   -> trace ("\nParse error " ++ show e ++ " on\n" ++ show txt) False
                            Right v' -> v == v'

-- | ConversionXML is Identity
prop_ConversionXML_id :: TestSortContext a => a -> Gen Bool
prop_ConversionXML_id ctx = all check <$> listOf1 (arbitraryValue ctx)
    where check :: Value -> Bool
          check v = 
                let xml = valueToXML ctx v
                    actual = valueFromXML ctx (getSort v) xml
                  in
                        case actual of
                            Left e   -> trace ("\nParse error " ++ show e ++ " on\n" ++ show xml) False
                            Right v' -> v == v'

spec :: Spec
spec =
  describe "conversion" $
    modifyMaxSuccess (const 100) $
    modifyMaxSize (const 20) $ 
      do
        it "fromText . toText == id" $ property (propertyInContext prop_ConversionText_id)
        it "fromXML . toXML == id" $ property (propertyInContext prop_ConversionXML_id)
