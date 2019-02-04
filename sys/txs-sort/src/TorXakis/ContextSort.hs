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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextSort
( -- * Sort Context
  ContextSort(ContextSort)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           GHC.Generics        (Generic)

import           TorXakis.Name       ( RefByName
                                     , toMapByName
                                     )
import           TorXakis.SortADT    ( ADTDef )
import           TorXakis.SortContext

-- | An instance of 'SortContext'.
newtype ContextSort = ContextSort { _adtDefs :: Map.Map (RefByName ADTDef) ADTDef 
                                  } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortReadContext ContextSort where
    adtDefs = _adtDefs

instance SortContext ContextSort where
    empty = ContextSort Map.empty
    addAdtDefs ctx as = case violationsAddAdtDefs ctx as of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { _adtDefs = Map.union (_adtDefs ctx) (toMapByName as) }
