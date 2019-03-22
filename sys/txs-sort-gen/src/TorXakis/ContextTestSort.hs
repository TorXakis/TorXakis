{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTestSort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Test Sort:
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextTestSort
(-- * Context Test Sort
  ContextTestSort
, TorXakis.ContextTestSort.empty
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, ConstructorDef
)
where
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.ContextSort
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.TestSortContext
import           TorXakis.TestSortData

-- | An instance of 'TestSortContext'.
data ContextTestSort = ContextTestSort
                        { basis :: ContextSort
                        , tsd :: TestSortData
                        } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty TestSortContext
empty :: ContextTestSort
empty = ContextTestSort TorXakis.ContextSort.empty TorXakis.TestSortData.empty

instance SortContext ContextTestSort where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case TorXakis.SortContext.addADTs as (basis ctx) of
                        Left e       -> Left e
                        Right basis' -> Right $ ContextTestSort basis' (TorXakis.TestSortData.afterAddADTs basis' as (tsd ctx))

instance TestSortContext ContextTestSort where
    sortSize s = TorXakis.TestSortData.sortSize s . tsd

    adtSize r = TorXakis.TestSortData.adtSize r . tsd

    constructorSize r c = TorXakis.TestSortData.constructorSize r c . tsd
