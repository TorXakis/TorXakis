{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  DistributeSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'NameGen'.
-----------------------------------------------------------------------------
module TorXakis.DistributeSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import TorXakis.Distribute

prop_sum :: NonNegative Int -> Positive Int -> Gen Bool
prop_sum (NonNegative n) (Positive m) = do
    dist <- distribute n m
    return $ n == sum dist

prop_length :: NonNegative Int -> NonNegative Int -> Gen Bool
prop_length (NonNegative n) (NonNegative m) = do
    dist <- distribute n m
    return $ m == length dist

spec :: Spec
spec =
  describe "A distribution" $ do
    it "For one or more bins: sum is equal to first parameter" $ property prop_sum
    it "length is equal to second parameter" $ property prop_length
