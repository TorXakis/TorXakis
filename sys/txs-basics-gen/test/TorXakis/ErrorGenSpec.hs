{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ErrorGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'Error'.
-----------------------------------------------------------------------------
module TorXakis.ErrorGenSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Cover
import           TorXakis.ErrorGen

-- | Eq check
prop_Eq :: ErrorGen -> Bool 
prop_Eq (ErrorGen a) = coverEq a

-- | Ord check
prop_Ord :: ErrorGen -> Bool 
prop_Ord (ErrorGen a) = coverOrd a

-- Read Show check
prop_ReadShow :: ErrorGen -> Bool
prop_ReadShow (ErrorGen val) = coverReadShow val

spec :: Spec
spec =
  describe "An Error" $ do
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow