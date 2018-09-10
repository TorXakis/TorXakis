{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SortGenContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- With Context Generate a 'TorXakis.Sort'
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.SortGenContext
(-- * Sort Gen Context
  SortGen (..)
, arbitrarySort
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)
import           Test.QuickCheck

import           TorXakis.Sort

import           TorXakis.NameGen
import           TorXakis.TestSortContext

-- | Definition of the sort generator.
newtype SortGen = SortGen { -- | accessor to 'TorXakis.Sort'
                            unSortGen :: Sort}
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Select an arbitrary Sort from the given context
arbitrarySort :: TestSortContext a => a -> Gen SortGen
arbitrarySort ctx =
    do
        n <- getSize
        let availableSort = Map.keys (Map.filter (<=n) (getMapSortSize ctx)) in
            SortGen <$> elements availableSort

-- | Generate ADTDefs that extend this context
arbitraryADTDefs :: TestSortContext a => a -> Gen [ADTDef]     -- TODO: ADTDefGen ?
arbitraryADTDefs ctx =
    do
        NameGen aName <- arbitrary :: Gen NameGen
        aNameGens <- arbitrary :: Gen (Set.Set NameGen)
        let names = Set.insert aName (Set.map unNameGen aNameGens) in
            
       