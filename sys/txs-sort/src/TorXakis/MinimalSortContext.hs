{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.MinimalSortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of Sort Context: MinimalSortContext
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.MinimalSortContext
( -- * Sort Context
  MinimalSortContext(MinimalSortContext)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           GHC.Generics        (Generic)

import           TorXakis.Error      ( MinError )
import           TorXakis.Name       ( RefByName
                                     , toMapByName
                                     )
import           TorXakis.SortADT    ( ADTDef )
import           TorXakis.SortContext

-- | A minimal instance of 'SortContext'.
newtype MinimalSortContext = MinimalSortContext { _adtDefs :: Map.Map (RefByName ADTDef) ADTDef 
                                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortSplit MinimalSortContext where
    empty = MinimalSortContext Map.empty
    adtDefs = _adtDefs

instance SortContext MinimalSortContext MinError where
    addAdtDefs ctx as = case violationsAddAdtDefs ctx as of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { _adtDefs = Map.union (_adtDefs ctx) (toMapByName as) }
