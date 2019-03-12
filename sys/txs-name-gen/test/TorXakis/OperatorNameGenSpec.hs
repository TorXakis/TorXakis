{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  OperatorNameGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'OperatorNameGen'.
-----------------------------------------------------------------------------
module TorXakis.OperatorNameGenSpec
(spec
)
where
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck
import           Text.Regex.TDFA

import           TorXakis.Cover
import           TorXakis.OperatorName
import           TorXakis.OperatorNameGen

-- | Eq check
prop_Eq :: OperatorNameGen -> Bool 
prop_Eq (OperatorNameGen a) = coverEq a

-- | Ord check
prop_Ord :: OperatorNameGen -> Bool 
prop_Ord (OperatorNameGen a) = coverOrd a

-- Read Show check
prop_ReadShow :: OperatorNameGen -> Bool
prop_ReadShow (OperatorNameGen val) = coverReadShow val

prop_notNull :: OperatorNameGen -> Bool
prop_notNull (OperatorNameGen nm) =
    ( not . T.null . toText ) nm

prop_Regex :: OperatorNameGen -> Bool
prop_Regex (OperatorNameGen nm) =
    T.unpack (toText nm) =~ "[-=+*/\\^<>|@&%]+"

spec :: Spec
spec =
  describe "A Generated OperatorName" $ do
    it "does not contain the empty string" $ property prop_notNull
    it "does adhere to regex pattern" $ property prop_Regex
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
