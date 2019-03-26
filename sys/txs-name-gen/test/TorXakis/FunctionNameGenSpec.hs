{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FunctionNameGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'FunctionNameGen'.
-----------------------------------------------------------------------------
module TorXakis.FunctionNameGenSpec
(spec
)
where
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Cover
import           TorXakis.FunctionName
import           TorXakis.FunctionNameGen
import           TorXakis.Language

-- | Eq check
prop_Eq :: FunctionNameGen -> Bool 
prop_Eq (FunctionNameGen a) = coverEq a

-- | Ord check
prop_Ord :: FunctionNameGen -> Bool 
prop_Ord (FunctionNameGen a) = coverOrd a

-- Read Show check
prop_ReadShow :: FunctionNameGen -> Bool
prop_ReadShow (FunctionNameGen val) = coverReadShow val

prop_notNull :: FunctionNameGen -> Bool
prop_notNull (FunctionNameGen nm) =
    ( not . T.null . TorXakis.FunctionName.toText ) nm

prop_Regex :: FunctionNameGen -> Bool
prop_Regex (FunctionNameGen nm) =
    satisfyTxsFuncOperator (TorXakis.FunctionName.toString nm)

spec :: Spec
spec =
  describe "A Generated FunctionName" $ do
    it "does not contain the empty string" $ property prop_notNull
    it "does adhere to regex pattern" $ property prop_Regex
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
