{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.CstrId
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for identifiers for constructors
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.Sort.CstrId
( CstrId (..)
)
where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           TorXakis.Sort.Id
import           TorXakis.Sort.Name
import           TorXakis.Sort.SortId

-- | Identifier for Constructor
data CstrId = CstrId
    { name     :: Name            -- capid
    , unid     :: Id
    , cstrargs :: [SortId]
    , cstrsort :: SortId
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | CstrId is Resettable
instance Resettable CstrId
-- | CstrId is Identifiable
instance Identifiable CstrId
