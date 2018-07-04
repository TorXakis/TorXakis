{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TorXakis.Sort.SortIdSpec where

import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Sort.SortIdGen

prop_SortIdEq :: SortIdGen -> Bool
prop_SortIdEq (SortIdGen val) =
    not (val /= val)

prop_SortIdOrd :: SortIdGen -> Bool
prop_SortIdOrd (SortIdGen val) =
    val >= val

prop_SortIdShow :: SortIdGen -> Bool
prop_SortIdShow (SortIdGen val) =
    show [val] == show [val]

spec :: Spec
spec =
  describe "A SortId"$ do
    it "derives Eq" $ property prop_SortIdEq
    it "derives Ord" $ property prop_SortIdOrd
    it "derives Show" $ property prop_SortIdShow