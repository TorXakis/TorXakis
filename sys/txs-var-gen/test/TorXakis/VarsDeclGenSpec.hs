{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarsDeclGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'TorXakis.VarsDeclGen'.
-----------------------------------------------------------------------------
module TorXakis.VarsDeclGenSpec
(spec
)
where
import           Data.List
import qualified Data.MultiMap            as MultiMap
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess,modifyMaxSize)
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl
import           TorXakis.VarsDeclGen

propertyInContext  :: (MinimalTestSortContext -> Gen Bool) -> Gen Bool
propertyInContext prop = do
    ctx <- arbitraryTestSortContext
    prop ctx

-- | Names in VarsDecl are unqiue
prop_UniqueNames :: TestSortContext a => a -> Gen Bool
prop_UniqueNames ctx = check <$> arbitraryVarsDecl ctx
    where check :: VarsDecl -> Bool
          check v = let l :: [VarDef]
                        l = toList v
                        ns :: [Name]
                        ns = map name l
                      in
                        ns == nub ns

-- | Sorts are defined within context
prop_DefinedSorts :: TestSortContext a => a -> Gen Bool
prop_DefinedSorts ctx = check <$> arbitraryVarsDecl ctx
    where check :: VarsDecl -> Bool
          check v = let l :: [VarDef]
                        l = toList v
                        ss :: [Sort]
                        ss = map (getSort ctx) l
                      in
                        all (elemSort ctx) ss

-- | toMultiMap clusters the elements
prop_SameElements :: TestSortContext a => a -> Gen Bool
prop_SameElements ctx = check <$> arbitraryVarsDecl ctx
    where check :: VarsDecl -> Bool
          check v = length (toList v) == MultiMap.size (toMultiMap v)

spec :: Spec
spec = do
  describe "constraints" $
    modifyMaxSuccess (const 100) $
    modifyMaxSize (const 20) $
      do
        it "unique names" $ property (propertyInContext prop_UniqueNames)
        it "defined sort" $ property (propertyInContext prop_DefinedSorts)
  describe "toMultiMap" $
    modifyMaxSize (const 20) $
        it "same elements" $ property (propertyInContext prop_SameElements)