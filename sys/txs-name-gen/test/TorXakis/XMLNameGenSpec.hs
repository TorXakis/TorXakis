{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMLNameGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'XMLNameGen'.
-----------------------------------------------------------------------------
module TorXakis.XMLNameGenSpec
(spec
)
where
import qualified Data.Text          as T
import           Test.Hspec
import           Test.QuickCheck
import           Text.Regex.TDFA

import           TorXakis.Cover
import           TorXakis.XMLName
import           TorXakis.XMLNameGen

-- | Eq check
prop_Eq :: XMLNameGen -> Bool 
prop_Eq (XMLNameGen a) = coverEq a

-- | Ord check
prop_Ord :: XMLNameGen -> Bool 
prop_Ord (XMLNameGen a) = coverOrd a

-- Read Show check
prop_ReadShow :: XMLNameGen -> Bool
prop_ReadShow (XMLNameGen val) = coverReadShow val

prop_notNull :: XMLNameGen -> Bool
prop_notNull (XMLNameGen nm) =
    ( not . T.null . toText ) nm

-- * The start character adheres to [A-Z] | \'_\' | [a-z]
-- * The remaining characters adhere to [A-Z] | \'_\' | [a-z] | \'-\' | [0-9]
prop_regex :: XMLNameGen -> Bool
prop_regex (XMLNameGen nm) =
    T.unpack (toText nm) =~ "[A-Z_a-z][A-Z_a-z0-9-]*"

spec :: Spec
spec =
  describe "A Generated XMLName" $ do
    it "does not contain the empty string" $ property prop_notNull
    it "adheres to the XML constraints" $ property prop_regex
    it "is an instance of Eq" $ property prop_Eq
    it "is an instance of Ord" $ property prop_Ord
    it "is an instance of Read and Show - read . show is identity" $ property prop_ReadShow
