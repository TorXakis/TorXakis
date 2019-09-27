{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BouteSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Boute'.
-----------------------------------------------------------------------------
module TorXakis.BouteSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Boute


prop_ModNonNegative :: Integer -> Integer -> Gen Bool
prop_ModNonNegative _ 0 = return True
prop_ModNonNegative m n = let r = TorXakis.Boute.mod m n in
                        return $ 0 <= r
                        
prop_ModInRange :: Integer -> Integer -> Gen Bool
prop_ModInRange _ 0 = return True
prop_ModInRange m n = let r = TorXakis.Boute.mod m n in
                        return $ r < abs n

prop_DivMod :: Integer -> Integer -> Gen Bool
prop_DivMod _ 0 = return True
prop_DivMod m n = let q = TorXakis.Boute.div m n
                      r = TorXakis.Boute.mod m n in
                        return $ m == q*n+r

spec :: Spec
spec = do
        describe "mod m n with n <> 0" $ do
            it "is always Non Negative (in [0, Inf)" $ property prop_ModNonNegative
            it "is always less than absolute value of divisor (in [0, abs n)" $ property prop_ModInRange
        describe "div and mod adhere to" $
            it "m = n * (div m n) + mod m n" $ property prop_DivMod