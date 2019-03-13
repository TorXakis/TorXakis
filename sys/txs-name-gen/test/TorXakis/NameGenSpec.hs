{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NameGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'NameGen'.
-----------------------------------------------------------------------------
module TorXakis.NameGenSpec
(spec
)
where
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Cover
import           TorXakis.Name
import           TorXakis.NameGen

-- | Eq check
prop_Eq :: NameGen -> Bool 
prop_Eq (NameGen a) = coverEq a

-- | Ord check
prop_Ord :: NameGen -> Bool 
prop_Ord (NameGen a) = coverOrd a

-- Read Show check
prop_ReadShow :: NameGen -> Bool
prop_ReadShow (NameGen val) = coverReadShow val

prop_notNull :: NameGen -> Bool
prop_notNull (NameGen nm) =
    ( not . T.null . toText ) nm

prop_notReservedName :: NameGen -> Bool
prop_notReservedName (NameGen nm) =
    ( not . isReservedToken . toText ) nm

prop_RepeatedByName_Set :: Set.Set NameGen -> Bool
prop_RepeatedByName_Set s =
    let input = Set.toList (Set.map unNameGen s) in
        null (repeatedByName input)

prop_RepeatedByName_Duplicate :: Set.Set NameGen -> Bool
prop_RepeatedByName_Duplicate s =
    let inputSet = Set.map unNameGen s
        inputList = Set.toList inputSet
        outputList = repeatedByName (inputList ++ inputList)
        outputSet = Set.fromList outputList in
            outputSet == inputSet

prop_RepeatedByNameIncremental_Duplicate :: Set.Set NameGen -> Set.Set NameGen -> Bool
prop_RepeatedByNameIncremental_Duplicate s1 s2 =
    let n1 = Set.map unNameGen s1
        n2 = Set.map unNameGen s2
        l1 = Set.toList n1
        l2 = Set.toList n2
        lo = repeatedByNameIncremental (l1 ++ l1) l2 
        no = Set.fromList lo in
            no == Set.intersection n1 n2

spec :: Spec
spec = do
  describe "A Generated Name" $ do
    it "does not contain the empty string" $ property prop_notNull
    it "does not contain a predefined string"$ property prop_notReservedName
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
  describe "RepeatedByName Function" $ do
    it "returns empty on any set" $ property prop_RepeatedByName_Set
    it "returns all duplicates" $ property prop_RepeatedByName_Duplicate
  describe "RepeatedByNameIncremental Function" $
    it "returns all duplicates in combination caused by second argument" $ property prop_RepeatedByNameIncremental_Duplicate