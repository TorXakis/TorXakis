{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

{-# LANGUAGE TemplateHaskell #-} 

module TestSum
(
    testSum
)
where

import Test.QuickCheck

import GenSum
import Sum

import Control.Arrow (second)
import qualified Data.List as List

-- -----------------------------------
-- deriving 
-- -----------------------------------
prop_SumMultiply :: GenSum Integer -> Integer -> Bool
prop_SumMultiply (GenSum s) 0 =  multiply 0 s == Sum.fromList []
prop_SumMultiply (GenSum s) n =  multiply n s == (fromDistinctAscMultiplierList . List.map (second (n *)) . toDistinctAscMultiplierList) s

prop_SumEq :: GenSum Integer -> Bool
prop_SumEq (GenSum s) =
    not (s /= s)

prop_SumOrd :: GenSum Integer -> Bool
prop_SumOrd (GenSum s) =
    s >= s

prop_SumShow :: GenSum Integer -> Bool
prop_SumShow (GenSum s) =
    show [s] == show [s]

return []
testSum :: IO Bool
testSum = $quickCheckAll