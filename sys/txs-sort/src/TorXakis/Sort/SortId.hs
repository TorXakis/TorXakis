{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.SortId
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structure for identifiers for sorts
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Sort.SortId
( SortId(..)
, sortIdBool
, sortIdInt
, sortIdString
, sortIdRegex
)
where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           TorXakis.Sort.Id
import           TorXakis.Sort.Name


-- | Identifier for Sort
data SortId = SortId
    { name :: Name
    , unid :: Id
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | SortId is Resettable
instance Resettable SortId

-- | SortId is Identifiable
instance Identifiable SortId

-- * standard sorts
-- | Identifier for Sort Bool
sortIdBool :: SortId
sortIdBool = SortId "Bool" 101

-- | Identifier for Sort Int
sortIdInt :: SortId
sortIdInt = SortId "Int" 102

-- | Identifier for Sort String
sortIdString :: SortId
sortIdString = SortId "String" 104

-- | Identifier for Sort Regex
sortIdRegex :: SortId
sortIdRegex = SortId "Regex" 105

