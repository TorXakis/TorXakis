{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings #-}
module SortId where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

-- Local imports.
import           Id
import           Name


data SortId = SortId
    { name :: Name            -- capid
    , unid :: Id
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable SortId
instance Identifiable SortId

-- * standard sorts

sortIdBool :: SortId
sortIdBool = SortId "Bool" 101

sortIdInt :: SortId
sortIdInt = SortId "Int" 102

sortIdString :: SortId
sortIdString = SortId "String" 104

sortIdRegex :: SortId
sortIdRegex = SortId "Regex" 105

sortIdError :: SortId
sortIdError = SortId "_Error" (-1)

