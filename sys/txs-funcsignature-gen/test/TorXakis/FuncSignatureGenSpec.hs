{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncSignatureGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'TorXakis.FuncSignatureGen'.
-----------------------------------------------------------------------------
module TorXakis.FuncSignatureGenSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureGen
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext

propertyInContext  :: (ContextTestSort -> Gen Bool) -> Gen Bool
propertyInContext prop = do
    ctx <- arbitraryTestSortContext
    prop ctx

-- | Sorts are defined within context
prop_DefinedSorts :: TestSortContext a => a -> Gen Bool
prop_DefinedSorts ctx = check <$> arbitraryFuncSignature ctx
    where check :: FuncSignature -> Bool
          check f = let l :: [Sort]
                        l = returnSort f : args f
                      in
                        all (memberSort ctx) l

spec :: Spec
spec =
  describe "constraints" $
        it "defined sort" $ property (propertyInContext prop_DefinedSorts)
