{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortGenContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortGenContext'.
-----------------------------------------------------------------------------
module TorXakis.SortGenContextSpec
(spec
)
where
import           Debug.Trace
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess,modifyMaxSize)
import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext

-- | Increments can be combined
prop_Increments :: Gen Bool
prop_Increments = 
    let c0 = empty :: MinimalTestSortContext in do
        incr1 <- arbitraryADTDefs c0
        case addAdtDefs c0 incr1 of
            Left e1  -> error ("Invalid generator 1 - " ++ show e1)
            Right c1 -> do
                            incr2 <- arbitraryADTDefs c1
                            case addAdtDefs c1 incr2 of
                                Left e2  -> error ("Invalid generator 2 - " ++ show e2)
                                Right c2 -> return $ case addAdtDefs c0 (incr2++incr1) of
                                                        Left e    -> trace ("error = " ++ show e) $ False
                                                        Right c12 -> if c12 == c2
                                                                        then True
                                                                        else trace ("incr1 = " ++ show incr1 ++ "\nincr2 = " ++ show incr2) $ False

spec :: Spec
spec =
  describe "A sort gen context" $
    modifyMaxSuccess (const 100) $
    modifyMaxSize (const 15) $
        it "incr2 after incr1 == incr2 ++ incr1" $ property prop_Increments
