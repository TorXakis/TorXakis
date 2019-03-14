{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarsDeclGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Generator for 'TorXakis.VarsDecl'.
-----------------------------------------------------------------------------
module TorXakis.VarsDeclGen
( -- | generator for arbitrary 'TorXakis.VarsDecl'
  arbitraryVarsDecl
  -- | to MultiMap by Sort
, toMultiMap
)
where

import           Data.Either
import qualified Data.MultiMap            as MultiMap
import qualified Data.Set                 as Set
import           Test.QuickCheck

import           TorXakis.NameGen
import           TorXakis.Sort (Sort)
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext
import           TorXakis.Var

-- | Generate VarsDecl within this context
arbitraryVarsDecl :: TestSortContext c => c -> Gen VarsDecl
arbitraryVarsDecl ctx = do
    nameGens <- arbitrary :: Gen (Set.Set NameGen)
    let names = Set.toList (Set.map unNameGen nameGens)

    sorts <- vectorOf (length names) (arbitrarySort ctx)
    let cs = zipWith (mkVarDef ctx) names sorts
    case partitionEithers cs of
        ([], vardefs) -> case mkVarsDecl ctx vardefs of
                            Right vd -> return vd
                            Left e  -> error ("Error in arbitraryVarsDecl: mkVarsDecl constructor failed\n" ++ show e)
        (es, _)       -> error ("Error in arbitraryVarsDecl: One of the mkVarDef constructors failed\n" ++ show es)

-- | toMultiMap By Sort
toMultiMap :: VarsDecl -> MultiMap.MultiMap Sort VarDef
toMultiMap = foldl insert MultiMap.empty . toList
    where insert :: MultiMap.MultiMap Sort VarDef -> VarDef -> MultiMap.MultiMap Sort VarDef
          insert m v = MultiMap.insert (sort v) v m