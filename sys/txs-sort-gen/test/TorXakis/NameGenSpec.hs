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

import           TorXakis.Sort
import           TorXakis.SortGen

prop_notNull :: NameGen -> Bool
prop_notNull (NameGen nm) =
    ( not . T.null . toText ) nm

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

spec :: Spec
spec = do
  describe "A Generated Name" $
    it "does not contain the empty string" $ property prop_notNull
  describe "RepeatedByName Function" $ do
    it "return empty on any set" $ property prop_RepeatedByName_Set
    it "return duplicates" $ property prop_RepeatedByName_Duplicate
