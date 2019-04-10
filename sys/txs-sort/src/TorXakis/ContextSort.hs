{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextSort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of Sort Context: ContextSort
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.ContextSort
( -- * Sort Context
  ContextSort(ContextSort)
, TorXakis.ContextSort.empty
)
where
import           Data.Data            (Data)
import           GHC.Generics         (Generic)

import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.Sort        ( Sort ( SortADT ) )
import           TorXakis.SortContext

-- | An instance of 'SortContext'.
newtype ContextSort = ContextSort { adtDefs :: NameMap ADTDef 
                                  } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty SortContext
empty :: ContextSort
empty = ContextSort TorXakis.NameMap.empty

instance SortContext ContextSort where
    memberSort (SortADT a) = member (toName a) . adtDefs
    memberSort _           = const True

    memberADT adtRef = member adtRef . adtDefs

    lookupADT adtRef = TorXakis.NameMap.lookup adtRef . adtDefs

    elemsADT = elems . adtDefs

    addADTs as ctx = case conceptualErrorAddADTs as ctx of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { adtDefs = adtDefs ctx `union` toNameMap as }
