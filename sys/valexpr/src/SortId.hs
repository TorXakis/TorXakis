{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortId
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
{-# LANGUAGE OverloadedStrings  #-}
module SortId where

import           Control.DeepSeq
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Data
import           GHC.Generics    (Generic)

-- Local imports.
import           Id
import           Name


-- | Identifier for Sort
data SortId = SortId
    { name :: Name            -- capid
    , unid :: Id
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance ToJSON SortId
instance FromJSON SortId

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

